{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as B 
import Data.Text
import Data.Int (Int32)
import Data.IORef (IORef, newIORef, atomicModifyIORef')

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Lucid.Html5
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Hasql.Connection

type MyDb =             Connection
type MyM =              SpockM MyDb MySession MyAppState ()
type MyAction =         SpockAction MyDb MySession MyAppState ()
type MyActionCtx ctx =  SpockActionCtx ctx MyDb MySession MyAppState ()

-- the session type is user id
-- empty when not connected

data MySession =
  EmptySession
    | SessionId Int32
    deriving (Show, Eq, Ord)

data MyAppState =       DummyAppState (IORef Int)

data Info = Info
  { cfgTitle :: Text
    , cfgDesc :: Text
    , cfgDomain :: Text
    , cfgDbConnStr :: B.ByteString
  } deriving (Show, Eq, Ord)

parseInfo :: FilePath -> IO (Info)
parseInfo file = do
  cfg <- C.load $ [C.Required file]
  name <- C.require cfg "name"
  desc <- C.require cfg "description"
  domain <- C.require cfg "domain"
  db <- C.require cfg "db"
  pure (Info name desc domain db)

hasqlPool :: B.ByteString -> ConnBuilder Connection
hasqlPool setting = ConnBuilder
  { cb_createConn = either (error . show . fmap B.unpack) id <$> acquire setting
  , cb_destroyConn = release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }

main :: IO ()
main = do
  info <- parseInfo "site.config"
  let dbconn = cfgDbConnStr info
  ref <- newIORef 0
  cfg <- defaultSpockCfg EmptySession (PCConn $ hasqlPool dbconn) (DummyAppState ref)
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
    doctypehtml_ $ do
      meta_ [ charset_ "utf-8" ]
      head_ $ do
        link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/base.css" ]
      body_ $ do
        h1_ "Hello"
        h1_ $ toHtml $ show i

