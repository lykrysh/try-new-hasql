module Types where

import Web.Spock
import Web.Spock.Config
import Hasql.Connection
import Data.IORef (IORef)
import Data.Int (Int32)
import Data.Text


type MyDb =             Connection
type MyM =              SpockM MyDb MySession MyAppState ()
type MyAction =         SpockAction MyDb MySession MyAppState ()
type MyActionCtx ctx a =  SpockActionCtx ctx MyDb MySession MyAppState a

data MySession = EmptySession
               | SessionId Int32
               deriving (Show, Eq, Ord)

data MyAppState = DummyAppState (IORef Int)

data Film = Film
  { filmId :: Int32
  , title :: Text
  , signature :: Text
  , author :: Text
  , year :: Int32
  } deriving (Show, Eq, Ord)
