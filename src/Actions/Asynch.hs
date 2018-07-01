module Actions.Asynch where

import Web.Spock
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Int (Int8)
import Data.Text (pack, Text)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Types.Base

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
