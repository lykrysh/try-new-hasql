module Actions.Asynch where

import Web.Spock
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Text (pack, Text)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Types.Base

filterAction :: Text -> Text -> MyAction
filterAction filter toggled = do
  filtersRef <- filters <$> getState
  liftIO $ atomicModifyIORef' filtersRef $ \f -> ([filter] <> [toggled] <> f, ())
