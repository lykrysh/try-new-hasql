module Actions.Asynch where

import Web.Spock (readSession, json)
import Hasql.Session (Error)
import Data.Text (pack, Text)
import Data.Int (Int32)
import Data.Aeson hiding (json)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Types.Base
import Types.Films
import qualified Db.Base as DB
import qualified Db.Films as DF

filterAction :: Text -> Text -> MyAction
filterAction filter toggled = do
  sess <- readSession
  case sess of
    EmptySession ->
      json $ object ["msg" .=  String "error"]
    SessionId sid -> do
      case toggled of
        "on" -> do
          turnedOn <- DB.writeQuery $ DF.toggleOnFilter sid filter
          case turnedOn of
            Left (pack . show -> e) ->
              json $ object [ "msg" .=  String e ]
            Right _ -> do
              retrieveOn sid
        "off" -> do
          turnedOff <- DB.writeQuery $ DF.toggleOffFilter sid filter
          case turnedOff of
            Left (pack . show -> e) ->
              json $ object [ "msg" .=  String e ]
            Right _ -> do
              retrieveOn sid
        _ ->
          json $ object [ "msg" .= String "don't know" ]

retrieveOn :: Int32 -> MyActionCtx () ()
retrieveOn sid = do
  filtersOn <- DB.readQuery $ DF.getOnFilters sid
  case filtersOn of
    Left (pack . show -> e) ->
      json $ object [ "msg" .= String e ]
    Right filters ->
      json $ object [ "filters" .= filters ]
