module Control.Monad.Freer.Sql.Types where

import Control.Monad.Freer (Member, Eff, send)

import Data.Typeable (Typeable)

import Database.Persist hiding (SelectOpt(..))
import qualified Database.Persist as P
import Database.Persist.Sql (SqlBackend)


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
    Insert :: (Typeable record, ToBackendKey SqlBackend record)
           => record -> Db (Key record)

    InsertMany :: (Typeable record, ToBackendKey SqlBackend record)
               => [record] -> Db [Key record]

    InsertUnique :: (Typeable record, ToBackendKey SqlBackend record)
                 => record -> Db (Maybe (Key record))

    Get :: (Typeable record, ToBackendKey SqlBackend record)
        => Key record -> Db (Maybe record)

    SelectList :: (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend)
               => [Filter record] -> [SelectOpt record] -> Db [Entity record]

    SelectFirst ::  (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend)
                => [Filter record] -> [SelectOpt record] -> Db (Maybe (Entity record))

    Update' :: (Typeable record, PersistEntity record, PersistEntityBackend record ~ SqlBackend)
            => Key record -> [Update record] -> Db ()


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
