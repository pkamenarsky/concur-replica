{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text              as T

import           Control.Monad (forever)

import           Concur.Core
import           Concur.Replica hiding (a, b, i)

import           Prelude hiding (div)

-- Demonstration of how easy it is to build a simple generic menu widget
-- 1. This uses no state, as it was easy to build this using monadic flow
-- 2. It was built by composing sub-widgets in a style that feels very functional.
--    Top, open, menuItem, and menuButton are legitimate widgets on their own.
menuWidget :: [(T.Text, [(a,T.Text)])] -> Widget HTML a
menuWidget cs = top 0 items >>= open items
  where
    items = menuItem <$> cs
    top i opts = orr $ zipWith (\(a,b) j -> a >>= \v -> return (b v,j)) opts [i..]
    open opts (b,i) =
      let w = [Left <$> top 0 (take i opts), Right <$> b, Left <$> top (i+1) (drop (i+1) opts)]
      in orr w >>= either (open opts) return
    menuItem (label', children) =
      ( div [className "menu"] [button [onClick] [text label']]
      , const $ div [className "menu"] (map menuButton children)
      )
    menuButton (ret,str) = ret <$ button [onClick] [text str]

main :: IO ()
main = runDefault 8080 "Menu" $ forever $ do
  v <- menuWidget items
  div [] $ [text $ "You picked - " <> v, button [onClick] [text "Restart"]]
  where
    items =
        [ ("Fruits",
            [ ("Apple","Apple")
            , ("Banana","Banana")
            ]
          )
        , ("Veggies",
            [ ("Tomato","Tomato")
            , ("Potato","Potato")
            ]
          )
        , ("Colors",
            [ ("Red","Red")
            , ("Green","Green")
            , ("Blue","Blue")
            , ("White","White")
            , ("Orange","Orange")
            ]
          )
        ]
