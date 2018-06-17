{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.IORef (IORef, newIORef, atomicModifyIORef')

import Init
import qualified Db as Db
import Html

type MyDb =             Db.Conn
type MyM =              SpockM MyDb MySession MyAppState ()
type MyAction =         SpockAction MyDb MySession MyAppState ()
type MyActionCtx ctx =  SpockActionCtx ctx MyDb MySession MyAppState ()

data MyAppState = DummyAppState (IORef Int)

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

rootAction :: MyAction
rootAction = do
  (DummyAppState ref) <- getState
  visitNum <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
  doCtx visitNum

doCtx :: Int -> MyActionCtx ()
doCtx i =
  lucid $ do
    renderTemplate
    renderNum i

