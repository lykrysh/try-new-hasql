module Types where

import Web.Spock
import Web.Spock.Config
import Hasql.Connection
import Data.Int (Int32)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text

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

data Event = Event
  { eventId :: Int32
    , eventName :: Text
    , eventDesc :: Text
    , eventLocation :: Text
  } deriving (Show, Eq, Ord)

data Attendant = Attendant
  { attendantUser :: Text
    , attendantAttending :: Bool
    , attendantFollowsChanges :: Bool
  } deriving (Show, Read, Eq, Ord)

type Attendants = Map Event [Attendant]
















{-
data Sign = Sign
  { sinLogin :: T.Text
  , sinPassword :: T.Text
  } deriving (Show)

signForm :: Monad m => D.Form HT m Sign
signForm = Sign
  <$> "login" D..: fmap (fmap trim) D.text Nothing
  <*> "password" D..: fmap (fmap trim) D.text Nothing

trim :: T.Text -> T.Text
trim = T.filter (/='\r') . T.reverse . T.dropWhile (==' ') . T.reverse . T.dropWhile (==' ')

signFormView :: D.View HT -> HT
signFormView view = do
  div_ $ do
    D.errorList "login" view
    D.label     "login" view "Name/Email: "
    D.inputText "login" view
  div_ $ do
    D.errorList "password" view
    D.label     "password" view "Password: "
    D.inputPassword "password" view
  D.inputSubmit "Sign"
  div_ $ do
    a_ [href_ "/lost-password" ] "I lost password"
-}
