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

getValidSessIdByIp :: Text -> Transaction (Maybe Int32)
getValidSessIdByIp ip = query ip $
  statement
    "select session_id from sessions where user_name=$1 and valid_until > now() order by valid_until desc limit 1"
    (SqlE.value SqlE.text)
    (SqlD.maybeRow (SqlD.value SqlD.int4))
    False

getValidSessIdByNum :: Int32 -> Transaction (Maybe Int32)
getValidSessIdByNum id = query id $
  statement
    "select session_id from sessions where session_id=$1 and valid_until > now() order by valid_until desc limit 1"
    (SqlE.value SqlE.int4)
    (SqlD.maybeRow (SqlD.value SqlD.int4))
    False

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
    False

toggleOnFilter :: SessFilter -> Transaction ()
toggleOnFilter sf = do
  query sf $ statement
    "insert into session_filters (session_id, filter) values ($1, $2)"
    encodeSessFilter
    SqlD.unit
    False

encodeSessFilter :: SqlE.Params SessFilter
encodeSessFilter =
  contramap sessionFilterId (SqlE.value SqlE.int4)
  <> contramap sessionFilter (SqlE.value SqlE.text)

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

