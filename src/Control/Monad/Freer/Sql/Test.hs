{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.Freer.Sql.Test where

import Control.Applicative ((<|>))

import Control.Monad.Freer (Eff, Member, Arr, handleRelayS)
import Control.Monad.Freer.Fresh (Fresh, fresh)
import Control.Monad.Freer.Sql.Types

import Data.Typeable hiding (typeOf, typeRep)
import qualified Data.Typeable as T

import Data.List (sortOn, findIndex)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromMaybe, catMaybes, listToMaybe)

import Data.Monoid (Endo(..))

import Database.Persist hiding (SelectOpt(..), insert, insertMany
                               ,get, selectList, selectFirst, update
                               ,insertUnique)
import Database.Persist.Sql (SqlBackend)


newtype Table record = Table (Map (Key record) record)

data SomeTable = forall record . (Typeable record) => SomeTable (Table record)

instance Show SomeTable where
    show _ = "SomeTable"

type MockDb = Map TypeRep SomeTable

data MkTable = forall a . (Typeable a, Ord (Key a), ToBackendKey SqlBackend a) => MkTable [(Int, a)]

typeRep :: forall a . Typeable a => TypeRep
typeRep = T.typeRep $ Proxy @a

makeDb :: [MkTable] -> MockDb
makeDb tables = M.fromList $ (\(MkTable table) -> mkTable table) <$> tables
    where mkTable :: forall a . (Typeable a, Ord (Key a), ToBackendKey SqlBackend a)
                  => [(Int, a)] -> (TypeRep, SomeTable)
          mkTable values = (typeRep @a, SomeTable . Table . M.mapKeys mkKey $ M.fromList values)

          mkKey :: (ToBackendKey SqlBackend a) => Int -> Key a
          mkKey = fromBackendKey . fromIntegral

mockGet :: forall record . (Typeable record, Ord (Key record)) => MockDb -> Key record -> Maybe record
mockGet db key = do
  SomeTable t <- M.lookup (typeRep @record) db
  Table t' <- gcast t
  M.lookup key t'

mockInsert :: forall record r . (ToBackendKey SqlBackend record, Typeable record, Ord (Key record), Member Fresh r)
           => MockDb -> record -> Eff r (MockDb, Key record)
mockInsert db record = do
  (backendKey :: BackendKey SqlBackend) <- fromIntegral <$> fresh
  let key = fromBackendKey backendKey
      newTable = fromMaybe (SomeTable . Table $ M.singleton key record) $ do
        SomeTable t <- M.lookup (typeRep @record) db
        Table t' <- gcast t
        return . SomeTable . Table $ M.insert key record t'
  return $ (M.insert (typeRep @record) newTable db, key)

mockInsertUnique :: forall record r
                  . (ToBackendKey SqlBackend record, Typeable record, Ord (Key record), Member Fresh r)
                 => MockDb -> record -> Eff r (Maybe (MockDb, Key record))
mockInsertUnique db record = do
  (backendKey :: BackendKey SqlBackend) <- fromIntegral <$> fresh
  let key = fromBackendKey backendKey
      mNewTable = fromMaybe (SomeTable . Table $ M.singleton key record) <$> do
        SomeTable t <- M.lookup (typeRep @record) db
        Table t' <- gcast t
        case filter (compareUnique record) (M.elems t') of
          [] -> return . Just . SomeTable . Table $ M.insert key record t'
          _ -> return Nothing
  case mNewTable of
    Just newTable -> return . Just $ (M.insert (typeRep @record) newTable db, key)
    _ -> return Nothing

  where compareUnique recA recB = any id $ zipWith (==) (extractUniqueValues recA) (extractUniqueValues recB)

        extractUniqueValues record' = persistUniqueToValues <$> persistUniqueKeys record'

mockSelectList :: forall record . (Typeable record, PersistEntity record)
               => MockDb -> [Filter record] -> [SelectOpt record] -> [Entity record]
mockSelectList db filters selectOpts = applySelectOpts selectOpts . fromMaybe [] $ do
  SomeTable t <- M.lookup (typeRep @record) db
  Table t' <- gcast t
  return . catMaybes $ checkFilter (FilterAnd filters) . uncurry Entity <$> M.toList t'

applySelectOpts :: (PersistEntity record) => [SelectOpt record] -> [Entity record] -> [Entity record]
applySelectOpts [] es = es
applySelectOpts (x : xs) es = applySelectOpts xs $ applySelectOpt x es

applySelectOpt :: (PersistEntity record) => SelectOpt record -> [Entity record] -> [Entity record]
applySelectOpt (Asc field) es = flip sortOn es $ toPersistValue . fst . fieldLens field (\v -> (v, v))
applySelectOpt (Desc field) es = reverse . flip sortOn es $ toPersistValue . fst . fieldLens field (\v -> (v, v))
applySelectOpt (OffsetBy n) es = drop n es
applySelectOpt (LimitTo n) es = take n es

checkFilter :: PersistEntity t => Filter t -> Entity t -> Maybe (Entity t)
checkFilter (Filter field (Left val) cmp) e = fieldLens field lens e
    where op Eq = (==)
          op Ne = (/=)
          op Gt = (>)
          op Lt = (<)
          op Ge = (>=)
          op Le = (<=)
          op _ = error "can't test this operation"

          lens val' = if op cmp (toPersistValue val') (toPersistValue val)
                      then Just val'
                      else Nothing
checkFilter (Filter field (Right vals) In) e = fieldLens field lens e
    where lens val = if toPersistValue val `elem` (toPersistValue <$> vals)
                     then Just val
                     else Nothing
checkFilter (Filter field (Right vals) NotIn) e = fieldLens field lens e
    where lens val = if toPersistValue val `elem` (toPersistValue <$> vals)
                     then Nothing
                     else Just val
checkFilter (FilterAnd fs) e = case sequence $ flip checkFilter e <$> fs of
                                 Just _ -> Just e
                                 Nothing -> Nothing
checkFilter (FilterOr []) _ = Nothing
checkFilter (FilterOr (f : fs)) e = checkFilter f e <|> checkFilter (FilterOr fs) e
checkFilter _ _ = error "can't test this filter"


mockUpdate :: forall record. (Typeable record, PersistEntity record, Ord (Key record))
           => MockDb -> Key record -> [Update record] -> MockDb
mockUpdate db key updates = fromMaybe db $ do
  SomeTable t <- M.lookup (typeRep @record) db
  Table t' <- gcast t
  return $ M.insert (typeRep @record) (SomeTable (Table (M.adjust applyUpdates key t'))) db

  where applyUpdates = appEndo . mconcat $ Endo . performUpdate <$> updates

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
