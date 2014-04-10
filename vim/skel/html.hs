{-# LANGUAGE OverloadedStrings #-}

import Prelude
import qualified Prelude as P
import Data.Monoid (mempty)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Angular

elem !. c = elem ! class_ c
elem !# i = elem ! A.id i

js uri = script ! src uri $ mempty
css uri = link ! rel "stylesheet" ! href uri

klass .$ innerHtml = H.div !. klass $ innerHtml
klass .! attr      = H.div !. klass ! attr

-- | Coerce that squirrely string literal
str :: String -> Html
str = toHtml

val :: String -> AttributeValue
val = toValue
-- -

main = putStrLn $ renderHtml $ do
