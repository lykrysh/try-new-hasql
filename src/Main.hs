module Main where

import Web.Spock
import Web.Spock.Config
import Hasql.Connection (Connection, acquire, release)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.IORef (newIORef)
import Types.Base
import Init
import Actions.Root
import Actions.Asynch

main :: IO ()
main = do
  info <- parseInfo "site.config"
  let dbconn = cfgDbConnStr info
  state <- MyAppState <$> newIORef []
  cfg <- defaultSpockCfg EmptySession (PCConn $ hasqlPool dbconn) state
  runSpock 8080 (spock cfg app)

hasqlPool :: ByteString -> ConnBuilder Connection
hasqlPool setting = ConnBuilder
  { cb_createConn = either (error . show . fmap unpack) id <$> acquire setting
  , cb_destroyConn = release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }

app :: MyM
app = do
  middleware (staticPolicy (addBase "static"))
  get root rootAction
  get "/filters" $ do
    filter <- param' "filter"
    toggled <- param' "toggled"
    filterAction filter toggled
