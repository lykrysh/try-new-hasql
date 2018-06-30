module Actions.Asynch where

import Web.Spock
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Int (Int8)
import Data.Text (pack, Text)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Hasql.Session (Error)
import Types.Base
import Types.Films
import qualified Db as Db

cipAction :: Text -> MyAction
cipAction client_ip = do
  filtersRef <- filters <$> getState

  -- make session client_ip 
  sessRes <- Db.readQuery $ Db.getValidSessionId client_ip
  case sessRes of
    Left (pack . show -> e) -> do
      liftIO $ atomicModifyIORef' filtersRef $ \f -> ([e] <> f, ())
    Right sr -> do
      case sr of
        Nothing -> do -- should make new session 
          newSess <- Db.writeQuery $ Db.newSession client_ip
          case newSess of
            Left (pack . show -> e) -> do
              liftIO $ atomicModifyIORef' filtersRef $ \f -> (["new"] <> [e] <> f, ())
            Right justid -> do
              case justid of
                Nothing ->
                  liftIO $ atomicModifyIORef' filtersRef $ \f -> (["new"] <> ["nothing"] <> f, ())
                Just id -> do
                  writeSession(SessionId id)
                  liftIO $ atomicModifyIORef' filtersRef $ \f -> (["new"] <> [client_ip] <> f, ())
        Just sessId -> do -- existing session
          existingSess <- Db.writeQuery $ Db.extendSession sessId
          case existingSess of
            Left (pack . show -> e) -> do
              liftIO $ atomicModifyIORef' filtersRef $ \f -> (["existing"] <> [e] <> f, ())
            Right justid -> do
              case justid of
                Nothing ->
                  liftIO $ atomicModifyIORef' filtersRef $ \f -> (["existing"] <> ["nothing"] <> f, ())
                Just id -> do
                  writeSession(SessionId id)
                  liftIO $ atomicModifyIORef' filtersRef $ \f -> (["existing"] <> [client_ip] <> f, ())



  sess <- readSession
  case sess of
    EmptySession -> 
      liftIO $ atomicModifyIORef' filtersRef $ \f -> (["EmptySession"] <> f, ())
    SessionId uid -> do
      liftIO $ atomicModifyIORef' filtersRef $ \f -> (["success"] <> f, ())
{-
filterAction :: Int8 -> Bool -> MyAction
filterAction keyId on = do
  filtersRef <- filters <$> getState
  case on of
    True -> do
      liftIO $ atomicModifyIORef' filtersRef $ \f -> ([keyId] <> f, ())
    False -> do
      liftIO $ atomicModifyIORef' filtersRef $ \f -> (removeItem keyId f, ())
        where removeItem _ [] = []
              removeItem x (y:ys) | x == y = removeItem x ys
                | otherwise = y : removeItem x ys
-}
