module Control.Monad.Freer.Sql
    ( insert
    , insertMany
    , insertUnique
    , get
    , selectFirst
    , selectList
    , update
    , sql
    , runSql
    , SQL
    , Db
    , Entity(..)
    , runDb
    , mockDb
    , SomeTable(..)
    , Table(..)
    , makeDb
    , MkTable(..)
    , module Database.Persist
    ) where

import Control.Applicative

import Control.Monad.Freer
import Control.Monad.Freer.Fresh

import Control.Monad.Reader

import Data.Typeable hiding (typeOf)

import Data.List (sortOn, findIndex)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe

import Data.Monoid

import Database.Persist hiding (SelectOpt(..), insert, insertMany
                               ,get, selectList, selectFirst, update
                               ,insertUnique)
import qualified Database.Persist as P
import Database.Persist.Sql (SqlBackend, ConnectionPool, runSqlPool)

data SQL a where
    SQL :: ReaderT SqlBackend IO a -> SQL a

sql :: (Member SQL r) => ReaderT SqlBackend IO a -> Eff r a
sql action = send $ SQL action

runSql :: (Member IO r) => ConnectionPool -> Eff (SQL ': r) v -> Eff r v
runSql pool eff = runNat (\(SQL s) -> runSqlPool s pool) eff

data SelectOpt record = forall typ. PersistField typ => Asc (EntityField record typ)
                      | forall typ. PersistField typ => Desc (EntityField record typ)
                      | OffsetBy Int
                      | LimitTo Int

toPersistSelectOpt :: SelectOpt record -> P.SelectOpt record
toPersistSelectOpt (Asc e) = P.Asc e
toPersistSelectOpt (Desc e) = P.Desc e
toPersistSelectOpt (OffsetBy n) = P.OffsetBy n
toPersistSelectOpt (LimitTo n) = P.LimitTo n


data Db a where
    Insert :: (Typeable record, ToBackendKey SqlBackend record) => record -> Db (Key record)
    InsertMany :: (Typeable record, ToBackendKey SqlBackend record) => [record] -> Db [Key record]
    InsertUnique :: (Typeable record, ToBackendKey SqlBackend record) => record -> Db (Maybe (Key record))
    Get :: (Typeable record, ToBackendKey SqlBackend record) => Key record -> Db (Maybe record)
    SelectList :: (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend) => [Filter record] -> [SelectOpt record] -> Db [Entity record]
    SelectFirst ::  (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend) => [Filter record] -> [SelectOpt record] -> Db (Maybe (Entity record))
    Update' :: (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend) => Key record -> [Update record] -> Db ()


insert :: (Typeable record, ToBackendKey SqlBackend record, Member Db r)
       => record -> Eff r (Key record)
insert record = send $ Insert record

insertMany :: (Typeable record, ToBackendKey SqlBackend record, Member Db r)
           => [record] -> Eff r [Key record]
insertMany records = send $ InsertMany records

insertUnique :: (Typeable record, ToBackendKey SqlBackend record, Member Db r)
             => record -> Eff r (Maybe (Key record))
insertUnique record = send $ InsertUnique record

get :: (Typeable record, ToBackendKey SqlBackend record, Member Db r)
    => Key record -> Eff r (Maybe record)
get key = send $ Get key

selectList :: (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend, Member Db r)
           => [Filter record] -> [SelectOpt record] -> Eff r [Entity record]
selectList filters selectOpts = send $ SelectList filters selectOpts

selectFirst :: (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend, Member Db r)
            => [Filter record] -> [SelectOpt record] -> Eff r (Maybe (Entity record))
selectFirst filters selectOpts = send $ SelectFirst filters selectOpts

update :: (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend, Member Db r)
       => Key record -> [Update record] -> Eff r ()
update key updates = send $ Update' key updates


runDb :: forall r v . (Member SQL r) => Eff (Db ': r) v -> Eff r v
runDb eff = handleRelay return (\action next -> next =<< handleDb action) eff
    where handleDb :: Db a -> Eff r a
          handleDb (Insert record) = sql $ P.insert record
          handleDb (InsertMany records) = sql $ P.insertMany records
          handleDb (InsertUnique record) = sql $ P.insertUnique record
          handleDb (Get key) = sql $ P.get key
          handleDb (SelectList filters selectOpts) = sql $ P.selectList filters (toPersistSelectOpt <$> selectOpts)
          handleDb (SelectFirst filters selectOpts) = sql $ P.selectFirst filters (toPersistSelectOpt <$> selectOpts)
          handleDb (Update' key updates) = sql $ P.update key updates


newtype Table record = Table (Map (Key record) record)

data SomeTable = forall record . (Typeable record) => SomeTable (Table record)

instance Show SomeTable where
    show _ = "SomeTable"

type MockDb = Map TypeRep SomeTable

data MkTable = forall a . (Typeable a, Ord (Key a), ToBackendKey SqlBackend a) => MkTable [(Int, a)]

makeDb :: [MkTable] -> MockDb
makeDb tables = M.fromList $ (\(MkTable table) -> mkTable table) <$> tables
    where mkTable :: forall a . (Typeable a, Ord (Key a), ToBackendKey SqlBackend a)
                  => [(Int, a)] -> (TypeRep, SomeTable)
          mkTable values = (typeRep (Proxy @a), SomeTable . Table . M.mapKeys mkKey $ M.fromList values)

          mkKey :: (ToBackendKey SqlBackend a) => Int -> Key a
          mkKey = fromBackendKey . fromIntegral

mockGet :: forall record . (Typeable record, Ord (Key record)) => MockDb -> Key record -> (Maybe record)
mockGet db key = case M.lookup (typeRep (Proxy @record)) db of
  Nothing -> Nothing
  Just (SomeTable t) -> case gcast t of
                          Nothing -> Nothing
                          Just (Table t') -> M.lookup key t'

mockInsert :: forall record r . (ToBackendKey SqlBackend record, Typeable record, Ord (Key record), Member Fresh r)
           => MockDb -> record -> Eff r (MockDb, Key record)
mockInsert db record = do
  (backendKey :: BackendKey SqlBackend) <- fromIntegral <$> fresh
  let key = fromBackendKey backendKey
  case M.lookup (typeRep (Proxy @record)) db of
    Nothing -> return $ (M.insert (typeRep (Proxy @record)) (SomeTable (Table (M.singleton key record))) db, key)
    Just (SomeTable t) -> case gcast t of
                            Nothing -> error "impossible"
                            Just (Table t') -> return $ (M.insert (typeRep (Proxy @record)) (SomeTable (Table (M.insert key record t'))) db, key)

mockInsertUnique :: forall record r
                  . (ToBackendKey SqlBackend record, Typeable record, Ord (Key record), Member Fresh r)
                 => MockDb -> record -> Eff r (Maybe (MockDb, Key record))
mockInsertUnique db record = do
  (backendKey :: BackendKey SqlBackend) <- fromIntegral <$> fresh
  let key = fromBackendKey backendKey
  case M.lookup (typeRep (Proxy @record)) db of
    Nothing -> return . Just $ (M.insert (typeRep (Proxy @record)) (SomeTable (Table (M.singleton key record))) db, key)
    Just (SomeTable t) -> case gcast t of
      Nothing -> error "impossible"
      Just (Table t') -> case filter (compareUnique record) (M.elems t') of
        [] -> return . Just $  (M.insert (typeRep (Proxy @record)) (SomeTable (Table (M.insert key record t'))) db, key)
        _ -> return Nothing

  where compareUnique recA recB = any id $ zipWith (==) (persistUniqueToValues <$> persistUniqueKeys recA) (persistUniqueToValues <$> persistUniqueKeys recB)

mockSelectList :: forall record . (Typeable record, PersistEntity record)
               => MockDb -> [Filter record] -> [SelectOpt record] -> [Entity record]
mockSelectList db filters selectOpts = applySelectOpts selectOpts $ case M.lookup (typeRep (Proxy @record)) db of
  Nothing -> []
  Just (SomeTable t) -> case gcast t of
                          Nothing -> []
                          Just (Table t') -> catMaybes $ checkFilter (FilterAnd filters) . uncurry Entity <$> M.toList t'



applySelectOpts :: (PersistEntity record) => [SelectOpt record] -> [Entity record] -> [Entity record]
applySelectOpts [] es = es
applySelectOpts (x : xs) es = applySelectOpts xs $ applySelectOpt x es

applySelectOpt :: (PersistEntity record) => SelectOpt record -> [Entity record] -> [Entity record]
applySelectOpt (Asc field) es = flip sortOn es $ toPersistValue . fst . fieldLens field (\v -> (v, v))
applySelectOpt (Desc field) es = reverse . flip sortOn es $ toPersistValue . fst . fieldLens field (\v -> (v, v))
applySelectOpt (OffsetBy n) es = drop n es
applySelectOpt (LimitTo n) es = take n es

checkFilter :: PersistEntity t => Filter t -> Entity t -> Maybe (Entity t)
checkFilter (Filter field (Left val) cmp) e = fieldLens field (\val' -> if op cmp (toPersistValue val') (toPersistValue val) then Just val' else Nothing) e
    where op Eq = (==)
          op Ne = (/=)
          op Gt = (>)
          op Lt = (<)
          op Ge = (>=)
          op Le = (<=)
          op _ = error "can't test this operation"
checkFilter (Filter field (Right vals) In) e = fieldLens field (\val -> if toPersistValue val `elem` (toPersistValue <$> vals) then Just val else Nothing) e
checkFilter (Filter field (Right vals) NotIn) e = fieldLens field (\val -> if toPersistValue val `elem` (toPersistValue <$> vals) then Nothing else Just val) e
checkFilter (FilterAnd fs) e = case sequence $ flip checkFilter e <$> fs of
                                 Just _ -> Just e
                                 Nothing -> Nothing
checkFilter (FilterOr []) _ = Nothing
checkFilter (FilterOr (f : fs)) e = checkFilter f e <|> checkFilter (FilterOr fs) e
checkFilter _ _ = error "can't test this filter"


mockUpdate :: forall record. (Typeable record, PersistEntity record, Ord (Key record))
           => MockDb -> Key record -> [Update record] -> MockDb
mockUpdate db key updates = case M.lookup (typeRep (Proxy @record)) db of
  Nothing -> db
  Just (SomeTable t) -> case gcast t of
    Nothing -> db
    Just (Table t') -> M.insert (typeRep (Proxy @record)) (SomeTable (Table (M.adjust (appEndo . mconcat $ Endo . performUpdate <$> updates) key t'))) db

performUpdate :: forall record. (PersistEntity record) => Update record -> record -> record
performUpdate (Update field val updateType) record = let
    persistVal = toPersistValue val
    fieldDef = persistFieldDef field
    mFieldIndex = findIndex (== fieldDef) (entityFields $ entityDef (Proxy @record))
    fields = (\(SomePersistField f) -> toPersistValue f) <$> toPersistFields record
    fields' = case mFieldIndex of
                Nothing -> fields
                Just fieldIndex -> updateNth fieldIndex (op updateType persistVal) fields
    eNewRecord = fromPersistValues fields'
    in case eNewRecord of
         Left _ -> record
         Right newRecord -> newRecord

    where updateNth :: Int -> (a -> a) -> [a] -> [a]
          updateNth _ _ [] = []
          updateNth 0 f (x : xs) = f x : xs
          updateNth n f (x : xs) = x : updateNth (n - 1) f xs

          op :: PersistUpdate -> PersistValue -> PersistValue -> PersistValue
          op Assign newVal _ = newVal
          op Add mod' val' = numUpdate (+) mod' val'
          op Subtract mod' val' = numUpdate (-) mod' val'
          op Multiply mod' val' = numUpdate (*) mod' val'
          op Divide (PersistInt64 mod') (PersistInt64 val') = PersistInt64 (val' `div` mod')
          op Divide (PersistDouble mod') (PersistDouble val') = PersistDouble (val' / mod')
          op Divide (PersistRational mod') (PersistRational val') = PersistRational (val' / mod')
          op _ _ _ = error "can't test this op"

          numUpdate :: (forall a . Num a => a -> a -> a) -> PersistValue -> PersistValue -> PersistValue
          numUpdate f (PersistInt64 mod') (PersistInt64 val') = PersistInt64 (val' `f` mod')
          numUpdate f (PersistDouble mod') (PersistDouble val') = PersistDouble (val' `f` mod')
          numUpdate f (PersistRational mod') (PersistRational val') = PersistRational (val' `f` mod')
          numUpdate _ _ val' = val'
performUpdate _ _ = error "can't test this update"

mockDb :: forall r v . (Member Fresh r) => MockDb -> Eff (Db ': r) v -> Eff r (MockDb, v)
mockDb initDb eff = handleRelayS initDb (\s a -> return (s, a)) handleDb eff
  where handleDb :: MockDb -> Db a -> (MockDb -> Arr r a (MockDb, v)) -> Eff r (MockDb, v)
        handleDb db (Get key) next = next db $ mockGet db key
        handleDb db (Insert record) next = do
          (db', key) <- mockInsert db record
          next db' key
        handleDb db (InsertUnique record) next = do
          mResult <- mockInsertUnique db record
          case mResult of
            Nothing -> next db Nothing
            Just (db', key) -> next db' (Just key)
        handleDb db (InsertMany []) next = next db []
        handleDb db (InsertMany (record : records)) next = do
           (db', key) <- mockInsert db record
           handleDb db' (InsertMany records) (\db'' rest -> next db'' (key : rest))
        handleDb db (SelectList fs ss) next = next db $ mockSelectList db fs ss
        handleDb db (SelectFirst fs ss) next = next db . listToMaybe $ mockSelectList db fs ss
        handleDb db (Update' key updates) next = next (mockUpdate db key updates) ()
