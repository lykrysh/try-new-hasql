module Db where

import Web.Spock.Config
import qualified Data.ByteString.Char8 as B 
import Hasql.Connection

type Conn = Connection

hasqlPool :: B.ByteString -> ConnBuilder Conn
hasqlPool setting = ConnBuilder
  { cb_createConn = either (error . show . fmap B.unpack) id <$> acquire setting
  , cb_destroyConn = release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }
