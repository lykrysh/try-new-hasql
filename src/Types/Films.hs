module Types.Films where

import Data.Int (Int32)
import Data.Text

data Film = Film
  { filmId :: Int32
  , title :: Text
  , signature :: Text
  , author :: Text
  , year :: Int32
  } deriving (Show, Eq, Ord)
