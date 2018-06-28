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
  state <- MyAppState <$> newIORef []
  cfg <- defaultSpockCfg EmptySession (PCConn $ Db.hasqlPool dbconn) state
  runSpock 8080 (spock cfg app)

app :: MyM
app = do
  middleware (staticPolicy (addBase "static"))
  get root rootAction
  get "/nonfiction" $ filterAction "nonfiction"
  get "/fiction" $ filterAction "fiction"
  get "/surreal" $ filterAction "surreal"
  get "/abstraction" $ filterAction "abstraction"
  get "/soundscape" $ filterAction "soundscape"


