module Db where

import Web.Spock
import Web.Spock.Config
import qualified Data.ByteString.Char8 as B 
import Hasql.Connection
import Hasql.Query
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Hasql.Session hiding (query)
import Hasql.Decoders as SqlD
import Types

type Conn = Connection

hasqlPool :: B.ByteString -> ConnBuilder Conn
hasqlPool setting = ConnBuilder
  { cb_createConn = either (error . show . fmap B.unpack) id <$> acquire setting
  , cb_destroyConn = release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }

readQuery :: Transaction a -> MyActionCtx v (Either Error a)
readQuery = runQuery . run . runReadTransaction

runReadTransaction :: Transaction a -> Session a
runReadTransaction = transaction Serializable Read

getNewFilms :: Transaction [Film]
getNewFilms = query () $
  statement
  "select * from films where signature='1';"
  mempty
  (SqlD.rowsList decodeFilm)
  False

decodeFilm :: SqlD.Row Film
decodeFilm = Film
  <$> SqlD.value SqlD.int4 -- filmId
  <*> SqlD.value SqlD.text -- title
  <*> SqlD.value SqlD.text -- signature
  <*> SqlD.value SqlD.text -- author
  <*> SqlD.value SqlD.int4 -- year

