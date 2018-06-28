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

  get ("/nonfiction" <//> "off") $ filterAction "nonfiction-off"
  get ("/fiction" <//> "off") $ filterAction "fiction-off"
  get ("/surreal" <//> "off") $ filterAction "surreal-off"
  get ("/abstraction" <//> "off") $ filterAction "abstraction-off"
  get ("/soundscape" <//> "off") $ filterAction "soundscape-off"

  get ("/nonfiction" <//> "on") $ filterAction "nonfiction-on"
  get ("/fiction" <//> "on") $ filterAction "fiction-on"
  get ("/surreal" <//> "on") $ filterAction "surreal-on"
  get ("/abstraction" <//> "on") $ filterAction "abstraction-on"
  get ("/soundscape" <//> "on") $ filterAction "soundscape-on"
