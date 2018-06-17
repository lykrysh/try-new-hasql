{-# LANGUAGE OverloadedStrings #-}
module Init where

import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as B 
import Data.Text
import Data.Int (Int32)

-- the session type is user id
-- empty when not connected

data MySession =
  EmptySession
    | SessionId Int32
    deriving (Show, Eq, Ord)

data Info = Info
  { cfgTitle :: Text
    , cfgDesc :: Text
    , cfgDomain :: Text
    , cfgDbConnStr :: B.ByteString
  } deriving (Show, Eq, Ord)

parseInfo :: FilePath -> IO (Info)
parseInfo file = do
  cfg <- C.load $ [C.Required file]
  name <- C.require cfg "name"
  desc <- C.require cfg "description"
  domain <- C.require cfg "domain"
  db <- C.require cfg "db"
  pure (Info name desc domain db)
