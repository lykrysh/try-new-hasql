{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Actions where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Web.Spock.Digestive (runForm)
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
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
  searched <- Db.readQuery $ Db.getSearchedFilms
  filtersRef <- filters <$> getState
  bummer <- liftIO $readIORef filtersRef

  form <- runForm "" RF.filtersForm
  case form of
    (view,Nothing) -> do
      lucid $ do
        RB.renderTemplate
        p_ $ toHtml $ show bummer
        RB.renderNewFilms newRelease
        D.form view "/" $ do
          RF.filtersFormView view
        RB.renderSearchedFilms searched
    (_, Just Filter { noCb0 }) -> do
      redirect "/"   

filterAction :: MyAction
filterAction = do
  filtersRef <- filters <$> getState
  liftIO $ atomicModifyIORef' filtersRef $ \f -> (f <> ["Hello"], ())
  redirect "/"
