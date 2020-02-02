{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import qualified Data.Text              as T

import           Control.Monad        (forever, void)
import           Control.Monad.State  (execStateT, get, put, lift)

import           Concur.Core
import           Concur.Replica

import           Prelude hiding (div)

type MenuItems a = [(a,T.Text)]

-- A Double menu, where the entries in the second menu depend on the first
doubleMenu :: T.Text -> T.Text -> MenuItems a -> (a -> MenuItems b) -> Widget HTML b
doubleMenu label1 label2 items f = menu1 >>= go
  where
    menu1 = menuWidget label1 items
    menu2 x = menuWidget label2 (f x)
    go x = orr [fmap Left menu1, fmap Right (menu2 x)] >>= either go return

-- A simple select menu
menuWidget :: T.Text -> MenuItems a -> Widget HTML a
menuWidget label' items = div [className "menu"]
  [ do
      _ <- button [onClick] [text label']
      orr $ map menuButton items
  ]
  where
    menuButton (ret,str) = ret <$ button [onClick] [text str]

-- State
data EntryState = EntryState { color :: T.Text, items :: [T.Text] }
data EntriesState = EntriesState { entries :: [EntryState] }

entryStateInit :: EntryState
entryStateInit = EntryState "Red" []
entriesStateInit :: Int -> EntriesState
entriesStateInit n = EntriesState $ replicate n entryStateInit

-- Widget that allows the user to add an item to an entry
entryWidget :: EntryState -> Widget HTML EntryState
entryWidget (EntryState {..}) = go color
  where
    go col' =
      div [className "main"]
        [ hr []
        , heading "Select a color"
        , Left <$> selColor
        , heading "Make entries"
        , Right <$> newEntry col'
        , heading "Current entries"
        , entriesList
        ]
      >>= either go (\e -> return (EntryState col' (e:items)))
    heading = h4 [] . (:[]) . text
    selColor = doubleMenu "Fruits" "Color" itemsFruit itemsColor
    newEntry col' = menuWidget ("New Entry for " <> col' <> " fruit") (itemsFruitColor col')
    entriesList = orr $ map (div [] . (:[]) . text) items

-- Main
main :: IO ()
main = void $ runDefault 8080 "MultiEntry" $ flip execStateT (entriesStateInit 5) $ forever $ do
    EntriesState {..} <- get
    (i', e') <- lift $ orr (renderEntry <$> zip [0..] entries)
    put $ EntriesState (take i' entries ++ [e'] ++ drop (i'+1) entries)
  where
    renderEntry (i',e) = (i',) <$> entryWidget e

-- Items for first menu
itemsFruit :: MenuItems T.Text
itemsFruit =
  [ ("Apple","Apple")
  , ("Banana","Banana")
  ]

-- Items for second menu
itemsColor :: T.Text -> MenuItems T.Text
itemsColor "Apple" =
  [ ("Red","Red")
  , ("Green","Green")
  ]
itemsColor "Banana" =
  [ ("Yellow","Yellow")
  , ("Green","Green")
  ]
itemsColor _ = itemsColor "Apple"

-- Items for selecting fruit from color
itemsFruitColor :: T.Text -> MenuItems T.Text
itemsFruitColor "Red" =
  [ ("Apple","Apple")
  ]
itemsFruitColor "Green" =
  [ ("Apple","Apple")
  , ("Banana","Banana")
  ]
itemsFruitColor "Yellow" =
  [ ("Banana","Banana")
  ]
itemsFruitColor _ = itemsFruitColor "Red"
