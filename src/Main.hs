module Main where

import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.IORef (newIORef)

import Types.Base
import Init
import qualified Db as Db
import Actions

main :: IO ()
main = do
  info <- parseInfo "site.config"
  let dbconn = cfgDbConnStr info
  ref <- newIORef 0
  cfg <- defaultSpockCfg EmptySession (PCConn $ Db.hasqlPool dbconn) (DummyAppState ref)
  runSpock 8080 (spock cfg app)

app :: MyM
app = do
  middleware (staticPolicy (addBase "static"))
  get root rootAction
  post "filter" filterAction
