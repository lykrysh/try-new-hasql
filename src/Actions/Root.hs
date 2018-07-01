module Actions.Root where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Data.IORef (atomicModifyIORef', readIORef)
import Control.Monad.IO.Class (liftIO)
import Types.Base
import qualified Db as Db
import qualified Render.Base as RB
import qualified Render.Filters as RF

import Lucid
import Lucid.Html5
import Network.Wai (Request, remoteHost)
import Data.Text
import Network.Socket (SockAddr)

getIp :: Request -> [Text]
getIp = do
  splitOn ":" . pack . show . remoteHost

rootAction :: MyAction
rootAction = do
  what <- request
  newRelease <- Db.readQuery $ Db.getNewFilms
  searched <- Db.readQuery $ Db.getSearchedFilms
  filtersRef <- filters <$> getState
  liftIO $ atomicModifyIORef' filtersRef $ \f -> (["hey"], ())
  bummer <- liftIO $ readIORef filtersRef

  lucid $ do
    RB.renderTemplate
--    p_ $ toHtml $ show bummer
    p_ $ toHtml $ show what
    RB.renderNewFilms newRelease
    RF.renderFilters
    RB.renderSearchedFilms searched
