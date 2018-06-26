module Actions where

import Web.Spock (getState)
import Web.Spock.Lucid (lucid)
import Data.IORef (atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)
import qualified Db as Db
import Types
import Render.Base
import Render.Forms

rootAction :: MyAction
rootAction = do
  (DummyAppState ref) <- getState
  visitNum <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
  newRelease <- Db.readQuery $ Db.getNewFilms
  searched <- Db.readQuery $ Db.getSearchedFilms
  lucid $ do
    renderTemplate
    renderNum visitNum
    renderNewFilms newRelease
    renderFilters "filter"
    renderSearchedFilms searched

filterAction :: MyActionCtx () ()
filterAction = do
  lucid $ do
    renderDummy
--  redirect "/"
