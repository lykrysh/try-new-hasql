module Types.Films where

import Data.Int (Int32)
import Data.Text
import Data.Time (UTCTime)

data Film = Film
  { filmId :: Int32
  , title :: Text
  , signature :: Text
  , author :: Text
  , year :: Int32
  } deriving (Show, Eq, Ord)

data UserSession = UserSession
  { sessionId :: Int32
  , userName :: Text
  , validUntil :: UTCTime
  } deriving (Show, Eq, Ord)

data SessFilter = SessFilter
  { sessionFilterId :: Int32
  , sessionFilter :: Text
  } deriving (Show, Eq, Ord)
