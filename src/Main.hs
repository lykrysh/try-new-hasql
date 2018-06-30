module Main where

import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.IORef (newIORef)
import Types.Base
import Init
import qualified Db as Db
import Actions.Root
import Actions.Asynch

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

  get "/whois" $ do
    cIp <- param' "ip"
    cipAction cIp
{-
  get ("/nonfiction" <//> "off") $ filterAction 1 False
  get ("/fiction" <//> "off") $ filterAction 2 False
  get ("/surreal" <//> "off") $ filterAction 3 False
  get ("/abstraction" <//> "off") $ filterAction 4 False
  get ("/soundscape" <//> "off") $ filterAction 5 False

  get ("/nonfiction" <//> "on") $ filterAction 1 True
  get ("/fiction" <//> "on") $ filterAction 2 True
  get ("/surreal" <//> "on") $ filterAction 3 True
  get ("/abstraction" <//> "on") $ filterAction 4 True
  get ("/soundscape" <//> "on") $ filterAction 5 True
-}
