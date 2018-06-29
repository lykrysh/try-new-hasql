module Actions where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Int (Int8)
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Types.Base
import qualified Db as Db
import qualified Render.Base as RB
import qualified Render.Filters as RF

import Lucid
import Lucid.Html5

rootAction :: MyAction
rootAction = do
  newRelease <- Db.readQuery $ Db.getNewFilms
  searched <- Db.readQuery $ Db.getSearchedFilms
  filtersRef <- filters <$> getState
--  liftIO $ atomicModifyIORef' filtersRef $ \f -> ([1..5], ())
  bummer <- liftIO $ readIORef filtersRef

  lucid $ do
    RB.renderTemplate
    p_ $ toHtml $ show bummer
    RB.renderNewFilms newRelease
    RF.renderFilters
    RB.renderSearchedFilms searched

cipAction :: Text -> MyAction
cipAction v = do
  filtersRef <- filters <$> getState
  liftIO $ atomicModifyIORef' filtersRef $ \f -> ([v] <> f, ())

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
