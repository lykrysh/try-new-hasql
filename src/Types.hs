module Types where

import Web.Spock
import Web.Spock.Config
import Hasql.Connection
import Data.Int (Int32)
import Data.IORef (IORef)

-- the session type is user id
-- empty when not connected

data MySession =
  EmptySession
    | SessionId Int32
    deriving (Show, Eq, Ord)

data MyAppState = DummyAppState (IORef Int)

type MyDb =                 Connection
type MyM =                  SpockM MyDb MySession MyAppState ()
type MyAction =             SpockAction MyDb MySession MyAppState ()
type MyActionCtx ctx a =    SpockActionCtx ctx MyDb MySession MyAppState a

