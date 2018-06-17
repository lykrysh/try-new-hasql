{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.IORef (IORef, newIORef, atomicModifyIORef')

import Types
import Init
import qualified Db as Db
import ActionCtx

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
  get root displayEvents
--  get root rootAction
  post ("event" <//> var <//> "attending") $ \(eid :: EventId) ->
    attendingAction eid (Just True)
  post ("event" <//> var <//> "not-attending") $ \(eid :: EventId) ->
    attendingAction eid (Just False)

rootAction :: MyAction
rootAction = do
  (DummyAppState ref) <- getState
  visitNum <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
  doCtx visitNum
