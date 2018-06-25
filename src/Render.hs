{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Render where

import Lucid
import Lucid.Html5
import Data.Text (pack)
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
      h1_ "Hello"

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


