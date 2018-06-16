{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Data.IORef (IORef, newIORef)

data MySession =        EmptySession
data MyAppState =       DummyAppState (IORef Int)

type MyM =              SpockM () MySession MyAppState ()
type MyAction =         SpockAction () MySession MyAppState ()
--type MyActionCtx ctx =  SpockActionCtx ctx () MySession MyAppState ()

main :: IO ()
main = do
  ref <- newIORef 0
  cfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock cfg app)

app :: MyM
app = do
  get root rootAction

rootAction :: MyAction
rootAction = do
  doCtx

doCtx :: SpockActionCtx () () MySession MyAppState ()
doCtx =
  lucid $ do
    h1_ "Hello"
