module Db.Base where

import Web.Spock (runQuery)
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Hasql.Session (Error, Session, run)
import Types.Base

readQuery :: Transaction a -> MyActionCtx v (Either Error a)
readQuery = runQuery . run . runReadTransaction

writeQuery :: Transaction a -> MyActionCtx v (Either Error a)
writeQuery = runQuery . run . runWriteTransaction

runReadTransaction :: Transaction a -> Session a
runReadTransaction = transaction Serializable Read

runWriteTransaction :: Transaction a -> Session a
runWriteTransaction = transaction Serializable Write
