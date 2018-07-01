{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Render.Base where

import Lucid
import Lucid.Html5
import Data.Text (pack, Text)
import Data.IORef (IORef, readIORef)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Hasql.Session (Error)
import Types.Films
import Types.Base

type HT = Html ()

renderTemplate :: HT
renderTemplate = do
  doctypehtml_ $ do
    meta_ [charset_ "utf-8" ]
    head_ $ do
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/base.css" ]
      script_ [ type_ "text/javascript", src_ "/js/jquery-3.2.1.min.js" ] (mempty :: Text)
    body_ ""

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
      h1_ "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"

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
