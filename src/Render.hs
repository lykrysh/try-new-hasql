{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Render where

import Lucid
import Lucid.Html5
import Web.Spock.Digestive
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import Data.Text (pack, Text)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Hasql.Session (Error)
import Types

type HT = Html ()

renderTemplate :: HT
renderTemplate = do
  doctypehtml_ $ do
    meta_ [charset_ "utf-8" ]
    head_ $ do
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/base.css" ]
    body_ $ do
      p_ "dummy"

renderNum :: Int -> HT
renderNum i = do
  h1_ $ toHtml $ show i

renderNewFilms :: (Either Error [Film]) -> HT
renderNewFilms filmList = do
  case filmList of
    Left (pack . show -> e) -> do
      h1_ $ toHtml e
    Right films -> do
      table_ $ do
        tr_ $ do
          th_ "film_id"
          th_ "title"
          th_ "signature"
          th_ "author"
          th_ "year"
        forM_ films $ \f -> tr_ $ do
          td_ $ toHtml $ show (filmId f)
          td_ $ toHtml (title f)
          td_ $ toHtml (signature f)
          td_ $ toHtml (author f)
          td_ $ toHtml $ show (year f)

renderFilters :: Text -> HT
renderFilters action =
  div_ [ class_ "filterbox" ] $ do
    form_ [ class_ "filters", action_ action, method_ "post" ] $ do
<<<<<<< HEAD
      input_ [ type_ "checkbox", value_ "1", id_ "cb0", name_ "cb0" ]
      label_ [ for_ "cb0" ] $ "no CB0"
      input_ [ type_ "checkbox", value_ "1", id_ "cb2", name_ "cb2" ]
      label_ [ for_ "cb2" ] $ "no CB2"
      input_ [ type_ "checkbox", value_ "1", id_ "cb3", name_ "cb3" ]
      label_ [ for_ "cb3" ] $ "no CB3"
      button_ [ class_ "checkout", type_ "submit" ] $ do
        img_ [ src_ "/img/refresh.png" ]

renderSearchedFilms :: (Either Error [Film]) -> HT
renderSearchedFilms filmList = div_ [ class_ "searched" ] $ do
  case filmList of
    Left (pack . show -> e) -> do
      h1_ $ toHtml e
    Right films -> do
      table_ $ do
        tr_ $ do
          th_ "film_id"
          th_ "title"
          th_ "signature"
          th_ "author"
          th_ "year"
        forM_ films $ \f -> tr_ $ do
          td_ $ toHtml $ show (filmId f)
          td_ $ toHtml (title f)
          td_ $ toHtml (signature f)
          td_ $ toHtml (author f)
          td_ $ toHtml $ show (year f)

renderDummy :: HT
renderDummy =
  h1_ "whatever"
