module Control.Monad.Freer.Sql.Internal where

import Control.Monad.Freer (Member, Eff, send, handleRelay, runNat)

import Control.Monad.Freer.Sql.Types

import Control.Monad.Reader (ReaderT(..))

import qualified Database.Persist as P
import Database.Persist.Sql (SqlBackend, ConnectionPool, runSqlPool)

data SQL a where
    SQL :: ReaderT SqlBackend IO a -> SQL a

sql :: (Member SQL r) => ReaderT SqlBackend IO a -> Eff r a
sql action = send $ SQL action

runSql :: (Member IO r) => ConnectionPool -> Eff (SQL ': r) v -> Eff r v
runSql pool eff = runNat (\(SQL s) -> runSqlPool s pool) eff

runDb :: forall r v . (Member SQL r) => Eff (Db ': r) v -> Eff r v
runDb eff = handleRelay return (\action next -> next =<< sql (handleDb action)) eff
    where handleDb :: Db a -> ReaderT SqlBackend IO a
          handleDb (Insert record) = P.insert record
          handleDb (InsertMany records) = P.insertMany records
          handleDb (InsertUnique record) = P.insertUnique record
          handleDb (Get key) = P.get key
          handleDb (SelectList filters selectOpts) = P.selectList filters (toPersistSelectOpt <$> selectOpts)
          handleDb (SelectFirst filters selectOpts) = P.selectFirst filters (toPersistSelectOpt <$> selectOpts)
          handleDb (Update' key updates) = P.update key updates
