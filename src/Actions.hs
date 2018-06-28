module Actions where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Data.IORef (atomicModifyIORef', readIORef)
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
  bummer <- liftIO $readIORef filtersRef

  lucid $ do
    RB.renderTemplate
    p_ $ toHtml $ show bummer
    RB.renderNewFilms newRelease
    RF.renderFilters
    RB.renderSearchedFilms searched

nonfictionAction :: MyAction
nonfictionAction = do
  filtersRef <- filters <$> getState
  liftIO $ atomicModifyIORef' filtersRef $ \f -> (f <> ["Non Fiction!!"], ())
