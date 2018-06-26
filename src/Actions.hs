module Actions where

import Web.Spock 
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
  form <- runForm "" RF.filtersForm
  searched <- Db.readQuery $ Db.getSearchedFilms
  lucid $ do
    RB.renderTemplate
    RB.renderNum visitNum
    RB.renderNewFilms newRelease
    RF.renderFForm form "filter"
    RB.renderSearchedFilms searched

filterAction :: MyAction
filterAction = do
  redirect "/"
