module Actions.Root where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Network.Wai (Request, remoteHost)
import qualified Data.List as L
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', readIORef)
import Control.Monad.IO.Class (liftIO)
import Types.Base
import qualified Db.Base as DB
import qualified Db.Films as DF
import qualified Render.Base as RB
import qualified Render.Filters as RF

import Data.Int (Int8)
import Data.Monoid ((<>))
import Hasql.Session (Error)

import Lucid
import Lucid.Html5

rootAction :: MyAction
rootAction = do
  newRelease <- DB.readQuery $ DF.getNewFilms
  searched <- DB.readQuery $ DF.getSearchedFilms

  cli <- request
  startSession $ getIp cli

  filtersRef <- filters <$> getState
  bummer <- liftIO $ readIORef filtersRef

  lucid $ do
    RB.renderTemplate
    p_ $ toHtml $ show bummer
    RB.renderNewFilms newRelease
    RF.renderFilters
    RB.renderSearchedFilms searched

startSession :: T.Text -> MyActionCtx () ()
startSession client_ip = do
  filtersRef <- filters <$> getState
  -- make session client_ip 
  sessRes <- DB.readQuery $ DF.getValidSessIdByIp client_ip
  case sessRes of
    Left (T.pack . show -> e) -> do
      liftIO $ atomicModifyIORef' filtersRef $ \f -> ([e] <> f, ())
    Right sr -> do
      case sr of
        Nothing -> do -- should make new session 
          newSess <- DB.writeQuery $ DF.newSession client_ip
          case newSess of
            Left (T.pack . show -> e) -> do
              liftIO $ atomicModifyIORef' filtersRef $ \f -> (["new"] <> [e] <> f, ())
            Right justid -> do
              case justid of
                Nothing ->
                  liftIO $ atomicModifyIORef' filtersRef $ \f -> (["new"] <> ["nothing"] <> f, ())
                Just id -> do
                  writeSession(SessionId id)
                  liftIO $ atomicModifyIORef' filtersRef $ \f -> (["new"] <> [client_ip] <> f, ())
        Just sessId -> do -- existing session
          existingSess <- DB.writeQuery $ DF.extendSession sessId
          case existingSess of
            Left (T.pack . show -> e) -> do
              liftIO $ atomicModifyIORef' filtersRef $ \f -> (["existing"] <> [e] <> f, ())
            Right _ -> do
              writeSession(SessionId sessId)
              liftIO $ atomicModifyIORef' filtersRef $ \f -> (["existing"] <> [client_ip] <> f, ())
  -- to check
  sess <- readSession
  case sess of
    EmptySession -> 
      liftIO $ atomicModifyIORef' filtersRef $ \f -> (["EmptySession"] <> f, ())
    SessionId uid -> do
      liftIO $ atomicModifyIORef' filtersRef $ \f -> ([T.pack $ show uid] <> f, ())

getIp :: Request -> T.Text
getIp = do
  L.head . T.splitOn ":" . T.pack . show . remoteHost
