module Actions where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Web.Spock.Digestive (runForm)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Types.Base
import qualified Db as Db
import qualified Render.Base as RB
import qualified Render.Forms as RF

import Lucid
import Lucid.Html5

rootAction :: MyAction
rootAction = do
  newRelease <- Db.readQuery $ Db.getNewFilms
  form <- runForm "" RF.filtersForm
  searched <- Db.readQuery $ Db.getSearchedFilms
  lucid $ do
    RB.renderTemplate
--    RB.renderNum visitNum
    RB.renderNewFilms newRelease
    RF.renderFForm form "filter"
    RB.renderSearchedFilms searched

filterAction :: MyAction
filterAction = do
  filtersRef <- filters <$> getState
  --liftIO $ atomicModifyIORef' filtersRef $ \f -> (f <> [Filter noCb0 noCb2 noCb3], ())
  bummer <- liftIO $ readIORef filtersRef
  lucid $ do
    p_ $ toHtml $ show bummer
  --redirect "/"
