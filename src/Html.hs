{-# LANGUAGE OverloadedStrings #-}
module Html where

import Lucid
import Lucid.Html5

renderTemplate :: Html ()
renderTemplate = do
  doctypehtml_ $ do
    meta_ [charset_ "utf-8" ]
    head_ $ do
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/base.css" ]
    body_ $ do
      h1_ "Hello"

renderNum :: Int -> Html ()
renderNum i = do
  h1_ $ toHtml $ show i
