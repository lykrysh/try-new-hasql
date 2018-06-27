module Types.Base where

import Web.Spock
import Web.Spock.Config
import Hasql.Connection
import Data.IORef (IORef)
import Data.Int (Int32)

type MyDb =             Connection
type MyM =              SpockM MyDb MySession MyAppState ()
type MyAction =         SpockAction MyDb MySession MyAppState ()
type MyActionCtx ctx a =  SpockActionCtx ctx MyDb MySession MyAppState a

data MySession = EmptySession
               | SessionId Int32
               deriving (Show, Eq, Ord)

--data MyAppState = DummyAppState (IORef Int)
data MyAppState = MyAppState
  { filters :: IORef [Filter]
  }

data Filter = Filter
  { noCb0 :: Bool
    , noCb2 :: Bool
    , noCb3 :: Bool
  } deriving (Show, Eq)
