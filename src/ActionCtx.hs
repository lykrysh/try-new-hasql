{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ActionCtx where

import Web.Spock
import Web.Spock.Lucid (lucid)
import Lucid
import Lucid.Html5
import Web.Spock.Digestive
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Data.Text as T
import Data.Int (Int32)
import Data.Time (UTCTime, DiffTime)
import Data.Monoid
import Control.Monad
import Types
import qualified Db as Db

type HT = Html ()
type EventId = Int32

attendingAction :: EventId -> Maybe Bool -> MyActionCtx () ()
attendingAction eid mIsAttending = do
  let user = "whoever" :: T.Text
  getResults <- Db.readQuery (Db.getEventById eid)
  case getResults of
    Left (T.pack . show -> e) -> do
      text e
    Right Nothing -> do
      text "Event not found"
    Right (Just event) -> do
      upsertResult <- Db.writeQuery $
        case mIsAttending of
          Just isAttending ->
            Db.upsertAttendant event (Attendant user isAttending isAttending)
          Nothing ->
            Db.removeAttendant event
      case upsertResult of
        Left (T.pack . show -> e) -> do
          text e
        Right _ ->
          redirect "/"

displayEvents :: MyAction
displayEvents = do
  mEventsAndAtts <- Db.readQuery $ do
    events <- Db.getEvents
    mapM (\e -> (\a -> (e,a)) <$> Db.getAttendentsForEvent e) events
  case mEventsAndAtts of
    Left (T.pack . show -> e) -> do
      text e
    Right eventsAA -> do
      lucid $ renderEvents "eVeNtS" eventsAA

renderEvents :: T.Text -> [(Event, [Attendant])] -> HT
renderEvents title eventsAndAtts =
  template title $ do
    case eventsAndAtts of
      [] -> p_ "No Available event"
      _ -> events eventsAndAtts

events :: [(Event, [Attendant])] -> HT
events =
  mapM_ (\(e,a) -> div_ [ class_ "attendants row" ] (event e *> attendants (eventId e) a) )

event :: Event -> HT
event e =
  div_ [ class_ "event nine colums" ] $ do
    h2_ $ do
      a_ [href_ ("/event/" <> (T.pack . show $ eventId e))] $
        toHtml $ eventName e
    table_ [ class_ "event-info" ] $ do
      tr_ $ do
        th_ "location"
        th_ "something else"
      mapM_ td_
        [ toHtml $ eventLocation e, "hello whatever" ]

attendants :: EventId -> [Attendant] -> HT
attendants eid atts =
  div_ [ class_ "attendants three columns" ] $ do
    div_ $ do
      h4_ "Are you going?"
      ul_ [ class_  "attending" ] . sequence_ $
        [ postlink
          ("/event/" <> T.pack (show eid) <> "/attending")
          "Yes"
        , " "
        , postlink
          ("/event/" <> T.pack (show eid) <> "/not-attending")
          "No"
        ]
    div_ $ do
      h4_ "Going"
      ul_ . mapM_ attendant . filter (attendantAttending) $ atts
    div_ $ do
      h4_ "Not Going"
      ul_ . mapM_ attendant . filter (not . attendantAttending) $ atts
    where
      attendant :: Attendant -> HT
      attendant (attendantUser -> u) = 
        li_ $ toHtml u

postlink :: T.Text -> T.Text -> HT
postlink action btn = do
  form_ [ class_ "postlink", action_ action, method_ "post" ] $ do
    input_ [ type_ "submit", value_ btn ] 

template :: T.Text -> HT -> HT
template title body = do
  doctypehtml_ $ do
    meta_ [charset_ "utf-8" ]
    head_ $ do
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/base.css" ]
      title_ $ toHtml title
    body_ $ do
      div_ [id_ "main" ] $ body

doCtx :: Int -> MyActionCtx () ()
doCtx i =
  lucid $ do
--    renderTemplate
    renderNum i

renderNum :: Int -> HT
renderNum i = do
  h1_ $ toHtml $ show i

