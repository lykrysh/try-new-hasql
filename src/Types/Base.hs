module Types.Base where

import Web.Spock
import Web.Spock.Config
import Hasql.Connection
import Data.IORef (IORef)
import Data.Int (Int8, Int32)
import Data.Text (Text)

type MyDb =             Connection
type MyM =              SpockM MyDb MySession MyAppState ()
type MyAction =         SpockAction MyDb MySession MyAppState ()
type MyActionCtx ctx a =  SpockActionCtx ctx MyDb MySession MyAppState a

data MySession = EmptySession
               | SessionId Int32
               deriving (Show, Eq, Ord)

data MyAppState = MyAppState
  { filters :: IORef [Text]
  }

