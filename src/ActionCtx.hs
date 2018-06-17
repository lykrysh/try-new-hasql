{-# LANGUAGE OverloadedStrings #-}
module ActionCtx where

import Web.Spock.Lucid (lucid)
import Lucid
import Lucid.Html5
import Web.Spock.Digestive
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Data.Text as T
import Types

type HT = Html ()

doCtx :: Int -> MyActionCtx () ()
doCtx i =
  lucid $ do
    renderTemplate
    renderNum i

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

data Sign = Sign
  { sinLogin :: T.Text
  , sinPassword :: T.Text
  } deriving (Show)

signForm :: Monad m => D.Form HT m Sign
signForm = Sign
  <$> "login" D..: fmap (fmap trim) D.text Nothing
  <*> "password" D..: fmap (fmap trim) D.text Nothing

trim :: T.Text -> T.Text
trim = T.filter (/='\r') . T.reverse . T.dropWhile (==' ') . T.reverse . T.dropWhile (==' ')

signFormView :: D.View HT -> HT
signFormView view = do
  div_ $ do
    D.errorList "login" view
    D.label     "login" view "Name/Email: "
    D.inputText "login" view
  div_ $ do
    D.errorList "password" view
    D.label     "password" view "Password: "
    D.inputPassword "password" view
  D.inputSubmit "Sign"
  div_ $ do
    a_ [href_ "/lost-password" ] "I lost password"

