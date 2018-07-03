module Db.Films where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Int (Int32)
import Data.Functor.Contravariant (contramap)
import Hasql.Query (statement)
import Hasql.Transaction (Transaction, query)
import Hasql.Decoders as SqlD
import Hasql.Encoders as SqlE
import Types.Films

{- session --}

getValidSessIdByIp :: Text -> Transaction (Maybe Int32)
getValidSessIdByIp ip = query ip $
  statement
    "select session_id from sessions where user_name=$1 and valid_until > now() order by valid_until desc limit 1"
    (SqlE.value SqlE.text)
    (SqlD.maybeRow (SqlD.value SqlD.int4))
    True

getValidSessIdByNum :: Int32 -> Transaction (Maybe Int32)
getValidSessIdByNum id = query id $
  statement
    "select session_id from sessions where session_id=$1 and valid_until > now() order by valid_until desc limit 1"
    (SqlE.value SqlE.int4)
    (SqlD.maybeRow (SqlD.value SqlD.int4))
    True

newSession :: Text -> Transaction (Maybe Int32)
newSession ip = do
  query ip $ statement
    "insert into sessions (user_name, valid_until) values ($1, now() + '2 days')"
    (SqlE.value SqlE.text)
    SqlD.unit
    True
  getValidSessIdByIp ip

extendSession :: Int32 -> Transaction ()
extendSession sessionid = do
  query sessionid $ statement
    "update sessions set valid_until=(now() + '2 days') where session_id=$1"
    (SqlE.value SqlE.int4)
    SqlD.unit
    True

{- session_filters -}

getSessFilter :: Int32 -> Text -> Transaction (Maybe SessFilter)
getSessFilter id filter =
  query (id, filter) $ statement
    "select * from session_filters where session_id = $1 and filter = $2"
    encodeSessFilter
    (SqlD.maybeRow decodeSessFilter)
    True

toggleOnFilter :: Int32 -> Text -> Transaction ()
toggleOnFilter id filter = do
  msf <- getSessFilter id filter
  case msf of
    Nothing ->
      query (id, filter) $ statement
        "insert into session_filters (session_id, filter, added_at) values ($1, $2, now())"
        encodeSessFilter
        SqlD.unit
        True
    Just _ -> -- this will never happen anyways
      query (id, filter) $ statement
        "update session_filters set added_at = now() where session_id = $1 and filter = $2"
        encodeSessFilter
        SqlD.unit
        True

toggleOffFilter :: Int32 -> Text -> Transaction (Either Text ())
toggleOffFilter id filter = do
  msf <- getSessFilter id filter
  case msf of
    Nothing ->
      pure $ Left "this filter not found"
    Just sf -> do
      r <- query ((sessionFilterId sf), (sessionFilter sf)) $ statement
        "delete from session_filters where session_id = $1 and filter = $2"
        encodeSessFilter
        SqlD.unit
        True
      pure $ pure r

{-
retrieveOnFilters :: Int32 -> Transaction (Maybe [SessFilter])
retrieveOnFilters id = do
  filters <- query id $
    statement
      "select * from session_filters where sesson_id = $1 order by added_at desc"
      (SqlE.value SqlE.int4)
      (SqlD.rowsList decodeSessFilter)
      True
-}

encodeSessFilter :: SqlE.Params (Int32, Text)
encodeSessFilter =
  (  contramap fst (SqlE.value SqlE.int4)
  <> contramap snd (SqlE.value SqlE.text)
  )

decodeSessFilter :: SqlD.Row SessFilter
decodeSessFilter = SessFilter
  <$> SqlD.value SqlD.int4 -- sessionid
  <*> SqlD.value SqlD.text -- filter

{- Retrieve film list -}

getNewFilms :: Transaction [Film]
getNewFilms = query () $
  statement
  "select * from films where signature='1';"
  mempty
  (SqlD.rowsList decodeFilm)
  False

getSearchedFilms :: Transaction [Film]
getSearchedFilms = query () $
  statement
  "select * from films where signature='0' or signature='2' or signature='3';"
  mempty
  (SqlD.rowsList decodeFilm)
  False

decodeSession :: SqlD.Row UserSession
decodeSession = UserSession
  <$> SqlD.value SqlD.int4 -- sessionId
  <*> SqlD.value SqlD.text -- userName
  <*> SqlD.value SqlD.timestamptz -- validUntil

decodeFilm :: SqlD.Row Film
decodeFilm = Film
  <$> SqlD.value SqlD.int4 -- filmId
  <*> SqlD.value SqlD.text -- title
  <*> SqlD.value SqlD.text -- signature
  <*> SqlD.value SqlD.text -- author
  <*> SqlD.value SqlD.int4 -- year

