module Actions where

import Web.Spock (getState)
import Web.Spock.Lucid (lucid)
import Web.Spock.Digestive (runForm)
import Data.IORef (atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)
import Types
import qualified Db as Db
import qualified Render.Base as RB
import qualified Render.Forms as RF

rootAction :: MyAction
rootAction = do
  (DummyAppState ref) <- getState
  visitNum <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
  newRelease <- Db.readQuery $ Db.getNewFilms
  searched <- Db.readQuery $ Db.getSearchedFilms
  form <- runForm "" RF.filtersForm
  lucid $ do
    RB.renderTemplate
    RB.renderNum visitNum
    RB.renderNewFilms newRelease
    RF.renderFForm form "filter"
    RB.renderSearchedFilms searched

filterAction :: MyActionCtx () ()
filterAction = do
  lucid $ do
    RB.renderDummy
--  redirect "/"
