{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Core (Widget)
import Concur.Replica
import Data.Maybe
import Data.Text (Text)
import Replica.VDOM (HTML)

import Prelude hiding (div)

selectApp :: Maybe Text -> Widget HTML a
selectApp mSelected = do
  newSelection <- div []
    [ select [onChange]
        [ option [disabled True, selected True, hidden True] [text "Select a choice..."]
        , option [value "A"] [text "Choose A"]
        , option [value "B"] [text "Choose B"]
        , option [value "C"] [text "Choose C"]
        ]
    , p [] [text ("Currently selected: " <> fromMaybe "none" mSelected)]
    ]
  selectApp (Just (eventToText newSelection))
  where
    eventToText :: BaseEvent -> Text
    eventToText = targetValue . target

main :: IO ()
main = runDefault 8080 "Select" (selectApp Nothing)
