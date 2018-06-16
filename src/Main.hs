{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection

data MySession =        EmptySession
data MyAppState =       DummyAppState (IORef Int)

type MyDb =             Connection
type MyM =              SpockM MyDb MySession MyAppState ()
type MyAction =         SpockAction MyDb MySession MyAppState ()
--type MyActionCtx ctx =  SpockActionCtx ctx MyDb MySession MyAppState ()

main :: IO ()
main = do
  ref <- newIORef 0
  cfg <- defaultSpockCfg EmptySession (PCConn $ hasqlPool setting) (DummyAppState ref)
  runSpock 8080 (spock cfg app)

app :: MyM
app = do
  get root rootAction

rootAction :: MyAction
rootAction = do
  (DummyAppState ref) <- getState
  visitNum <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
  doCtx visitNum

doCtx :: Int -> SpockActionCtx () MyDb MySession MyAppState ()
doCtx i =
  lucid $ do
    h1_ "Hello"
    h1_ $ toHtml $ show i

setting :: DB.ByteString
setting = "host=localhost dbname=hasqldb port=5432 user=hasqluser password=password"

hasqlPool :: Settings -> ConnBuilder Connection
hasqlPool setting = ConnBuilder
  { cb_createConn = either (error . show . fmap DBC.unpack) id <$> acquire setting
  , cb_destroyConn = release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }
