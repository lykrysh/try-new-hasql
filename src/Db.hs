{-# LANGUAGE OverloadedStrings #-}

module Db where

import Web.Spock
import Web.Spock.Config
import qualified Data.ByteString.Char8 as B 
import Hasql.Connection
import Hasql.Query
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Hasql.Session hiding (query)
import Hasql.Decoders as SqlD
import Hasql.Encoders as SqlE
import Data.Int (Int32)
import Data.Monoid
import Data.Functor.Contravariant (contramap)
import Types

readQuery :: Transaction a -> MyActionCtx v (Either Error a)
readQuery = runQuery . run . runReadTransaction

runReadTransaction :: Transaction a -> Session a
runReadTransaction = transaction Serializable Read

writeQuery :: Transaction a -> MyActionCtx v (Either Error a)
writeQuery = runQuery . run . runWriteTransaction

runWriteTransaction :: Transaction a -> Session a
runWriteTransaction = transaction Serializable Write

getEvents :: Transaction [Event]
getEvents = query () $
  statement
    "select * from events"
    mempty
    (SqlD.rowsList decodeEvent)
    False

decodeEvent :: SqlD.Row Event
decodeEvent = Event
  <$> SqlD.value SqlD.int4 -- id
  <*> SqlD.value SqlD.text -- name
  <*> SqlD.value SqlD.text -- description
  <*> SqlD.value SqlD.text -- location

getAttendentsForEvent :: Event -> Transaction [Attendant]
getAttendentsForEvent event = do
  query event $
    statement
      "select a.user_id, a.attending, a.follow_changes from attendants a where a.event_id = $1"
      (eventId `contramap` SqlE.value SqlE.int4)
      (SqlD.rowsList decodeAttendant)
      True

decodeAttendant :: SqlD.Row Attendant
decodeAttendant = Attendant
  <$> SqlD.value SqlD.text -- userid
  <*> SqlD.value SqlD.bool -- attending
  <*> SqlD.value SqlD.bool -- follows changes

getEventById :: Int32 -> Transaction (Maybe Event)
getEventById eid = query eid $
  statement
   "select * from events where event_id = $1"
   (SqlE.value SqlE.int4)
   (SqlD.maybeRow decodeEvent)
   False

upsertAttendant :: Event -> Attendant -> Transaction ()
upsertAttendant event att = query (event, att) $
  statement
    "insert into attendants (event_id, user_id, attending, follow_changes) values (4, 'whoever', true, true)"
    encodeAttendant
    SqlD.unit
    True

encodeAttendant :: SqlE.Params (Event, Attendant)
encodeAttendant = 
  contramap (eventId . fst) (SqlE.value SqlE.int4)
  <> contramap (attendantUser . snd) (SqlE.value SqlE.text)
  <> contramap (attendantAttending . snd) (SqlE.value SqlE.bool)
  <> contramap (attendantFollowsChanges. snd) (SqlE.value SqlE.bool)

removeAttendant :: Event -> Transaction ()
removeAttendant event  = query (event) $
  statement
    "delete from attendants where event_id = $1"
    encodeEventUserIds
    SqlD.unit
    True

encodeEventUserIds :: SqlE.Params (Event)
encodeEventUserIds = contramap (eventId) (SqlE.value SqlE.int4)

hasqlPool :: B.ByteString -> ConnBuilder Connection
hasqlPool setting = ConnBuilder
  { cb_createConn = either (error . show . fmap B.unpack) id <$> acquire setting
  , cb_destroyConn = release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }
