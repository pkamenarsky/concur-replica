{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Core (Widget)
import Concur.Replica
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Replica.VDOM (HTML)

import Prelude hiding (div)

import qualified Concur.Replica.SVG as S
import qualified Concur.Replica.SVGProps as SP
import qualified Data.Text as T

import Control.Concurrent

selectApp :: Maybe Text -> Widget HTML a
selectApp mSelected = do
  _ <-
    div []
    [ p [] [text "You should see a black rectangle for one second..."]
    , S.svg [width "200", height "200", SP.version "1.1", SP.xmlns]
        [ S.rect [SP.x "10", SP.y "10", width "180", height "180"]
            []
        ]
    , liftIO (threadDelay 2000000)
    ]
  div []
    [ p [] [text "Now you should see a small red rectangle."]
    , S.svg [width "200", height "200", SP.version "1.1", SP.xmlns]
        [ S.rect [SP.x "60", SP.y "60", width "80", height "80", SP.fill "red"]
            []
        ]
    ]

main :: IO ()
main = runDefault 8080 "SVG" (selectApp Nothing)
