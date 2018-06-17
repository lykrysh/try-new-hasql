{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Lucid.Html5
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Db as Db

data MySession =        EmptySession
data MyAppState =       DummyAppState (IORef Int)

type MyM =              SpockM Db.MyDb MySession MyAppState ()
type MyAction =         SpockAction Db.MyDb MySession MyAppState ()
--type MyActionCtx ctx =  SpockActionCtx ctx Db.MyDb MySession MyAppState ()

main :: IO ()
main = do
  ref <- newIORef 0
  cfg <- defaultSpockCfg EmptySession (PCConn $ Db.hasqlPool Db.setting) (DummyAppState ref)
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

doCtx :: Int -> SpockActionCtx () Db.MyDb MySession MyAppState ()
doCtx i =
  lucid $ do
    doctypehtml_ $ do
      meta_ [ charset_ "utf-8" ]
      head_ $ do
        link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/base.css" ]
      body_ $ do
        h1_ "Hello"
        h1_ $ toHtml $ show i
