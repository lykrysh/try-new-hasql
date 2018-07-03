module Actions.Asynch where

import Web.Spock (readSession, text)
import Hasql.Session (Error)
import Data.Text (pack, Text)
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
      text "error"
    SessionId sid -> do
      case toggled of
        "on" -> do
          turnedOn <- DB.writeQuery $ DF.toggleOnFilter sid filter
          case turnedOn of
            Left (pack . show -> e) -> do
              text e
            Right _ -> do
              text "done"
        "off" -> do
          text "off"
            -- delete where session_id, filter
        _ -> do
          text "don't know"
            -- error

