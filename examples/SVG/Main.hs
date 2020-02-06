{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Core (Widget)
import Concur.Replica
import Prelude hiding (div)
import Replica.VDOM (HTML)

import qualified Concur.Replica.DOM.Events as SE
import qualified Concur.Replica.SVG as S
import qualified Concur.Replica.SVG.Props as SP

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"

app :: Widget HTML a
app = do
  _ <-
    div []
    [ p [] [text "Click to change size and color"]
    , S.svg [SE.onClick, width "200", height "200", SP.version "1.1", xmlns]
        [ S.rect [SP.x "10", SP.y "10", width "180", height "180"]
            []
        ]
    ]
  div []
    [ p [] [text "Now you should see a small red rectangle."]
    , S.svg [width "200", height "200", SP.version "1.1", xmlns]
        [ S.rect [SP.x "60", SP.y "60", width "80", height "80", SP.fill "red"]
            []
        ]
    ]

main :: IO ()
main = runDefault 8080 "SVG" app
