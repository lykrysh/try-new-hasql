{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Render.Forms where

import Lucid
import Lucid.Html5
import Text.Digestive ((.:))
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import Data.Text (Text)
import Types.Base
import qualified Db as Db
import qualified Render.Base as RB

type HT = Html ()

filtersForm :: Monad m => D.Form HT m Filter
filtersForm = Filter
  <$> "noCb0" .: D.bool (Just True)
  <*> "noCb2" .: D.bool Nothing
  <*> "noCb3" .: D.bool Nothing

filtersFormView :: D.View HT -> HT
filtersFormView view =
  div_ [ class_ "filterbox" ] $ do
    D.inputCheckbox "noCb0" view
    D.label "noCb0" view "no Cb0"
    D.inputCheckbox "noCb2" view
    D.label "noCb2" view "no Cb2"
    D.inputCheckbox "noCb3" view
    D.label "noCb3" view "no Cb3"
--    button_ [ class_ "checkout", type_ "submit" ] $ do
--      img_ [ src_ "/img/refresh.png" ]
    D.inputSubmit "Save"
