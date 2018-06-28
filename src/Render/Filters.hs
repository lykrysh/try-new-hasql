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
  div_ [ class_ "kwcontainer" ] $ do
    h1_ "whatever"
