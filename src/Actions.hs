module Actions where

import Web.Spock (getState)
import Web.Spock.Lucid (lucid)
import Data.IORef (atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)
import qualified Db as Db
import Types
import Render

rootAction :: MyAction
rootAction = do
  (DummyAppState ref) <- getState
  visitNum <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
  newRelease <- Db.readQuery $ Db.getNewFilms
  lucid $ do
    renderTemplate
    renderNum visitNum
    renderNewFilms newRelease

