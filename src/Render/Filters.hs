{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Render.Filters where

import Lucid
import Lucid.Html5
import Data.Text (Text)

type HT = Html ()

renderFilters :: HT
renderFilters = do
  div_ [ class_ "kwcontainer noselect" ] $ do
    a_ [ data_ "stateName" "nonfiction", class_ "kwbutcommon but pntcursor", id_ "nonfiction" ] $ do
      "Nonfiction"
    a_ [ data_ "stateName" "fiction", class_ "kwbutcommon but pntcursor", id_ "fiction" ] $ do
      "Fiction"
    a_ [ data_ "stateName" "surreal", class_ "kwbutcommon but pntcursor", id_ "surreal" ] $ do
      "Surreal"
    a_ [ data_ "stateName" "abstraction", class_ "kwbutcommon but pntcursor", id_ "abstraction" ] $ do
      "Abstraction"
    a_ [ data_ "stateName" "soundscape", class_ "kwbutcommon but pntcursor", id_ "soundscape" ] $ do
      "Soundscape"
  div_ [ id_ "info" ] $ do
    h1_ "Something else"
  script_ [ type_ "text/javascript", src_ "/js/ajax.js" ] (mempty :: Text)

