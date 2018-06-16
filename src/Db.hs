{-# LANGUAGE OverloadedStrings #-}
module Db where

import Web.Spock.Config
import qualified Data.ByteString.Char8 as DBC
import Hasql.Connection

type MyDb = Connection

setting :: DBC.ByteString
setting = "host=localhost dbname=hasqldb port=5432 user=hasqluser password=password"

hasqlPool :: Settings -> ConnBuilder Connection
hasqlPool setting = ConnBuilder
  { cb_createConn = either (error . show . fmap DBC.unpack) id <$> acquire setting
  , cb_destroyConn = release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }
