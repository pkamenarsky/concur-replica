{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Maybe (mapMaybe)
import qualified Data.Text as T

import           Control.Exception
import           Control.Monad.IO.Class
import           Concur.Replica hiding (i)

import qualified Prelude as P
import           Prelude hiding (div)

data Todo = Todo
  { todoValue :: T.Text
  , todoDone :: Bool
  , todoId :: Int
  } deriving Show

inputOnEnter :: T.Text -> UI HTML T.Text
inputOnEnter v = do
  ev <- input [ autofocus True, placeholder "What needs to be done?", value v, Left <$> onInput, Right <$> onKeyDown ]
  case ev of
    Left e  -> inputOnEnter (targetValue $ target e)
    Right e -> if kbdKey e == "Enter"
      then pure v
      else inputOnEnter v

todo :: Todo -> UI HTML Todo
todo v = do
  ev <- div
    [ key $ T.pack $ show (todoId v), Left <$> onDoubleClick
    , style
        [ ("color", if todoDone v then "#ccc" else "#333")
        ]
    ]
    [ input [ type_ "checkbox", checked (todoDone v), Right <$> onClick ]
    , text (todoValue v)
    ]
  case ev of
    Right _ -> pure $ v { todoDone = not (todoDone v) }
    Left _  -> do
      e <- div [] [ inputOnEnter (todoValue v) ]
      pure $ v { todoValue = e }

todoList :: T.Text -> [Todo] -> UI HTML [Todo]
todoList flt vs = do
  (i, newEntry) <- div []
    [ orr $ flip mapMaybe (zip [0..] vs) $ \(i, v) -> if flt `T.isInfixOf` todoValue v
        then Just ((i,) <$> (todo v))
        else Nothing
    ]
  pure (take i vs ++ [newEntry] ++ drop (i + 1) vs)

data OneOf3 a b c = One3 a | Two3 b | Three3 c

todos :: UI HTML a
todos = go 0 "" []
  where
    go vid filter' vs = do
      ev <- div []
        [ One3   <$> inputOnEnter ""
        , Two3   <$> input [ placeholder "Filter todos",  value filter', onInput ]
        , Three3 <$> todoList filter' vs
        ]
      case ev of
        One3 e    -> go (vid + 1) filter' (Todo e False vid:vs)
        Two3 e    -> go vid (targetValue $ target e) vs
        Three3 vs' -> go vid filter' vs'

--------------------------------------------------------------------------------

counter :: Int -> UI HTML a
counter x = do
  click <- div [ className "test" ]
    [ Left <$> div [ onClick ] [ text "-" ]
    , text $ T.pack $ show x
    , Right <$> div [ onClick ] [ text "+" ]
    ]

  case click of
    Left _  -> counter (x - 1)
    Right _ -> counter (x + 1)

--------------------------------------------------------------------------------

clientSidePrediction :: T.Text -> UI HTML a
clientSidePrediction v = do
  e <- div []
    [ input [ onInput, value v ]
    , text v
    , text v
    , text v
    , text v
    ]

  if targetValue (target e) == "delete"
    then clientSidePrediction ""
    else clientSidePrediction $ targetValue (target e)

--------------------------------------------------------------------------------

mouseEnterLeave :: UI HTML a
mouseEnterLeave = go False
  where
    go inside = do
      e <- div
        [ style
          [ ("position", "relative")
          , ("backgroundColor", if inside then "#fb0" else "#333")
          , ("top", "20px")
          , ("left", "20px")
          , ("width", "20px")
          , ("height", "20px")
          ]
        , Left <$> onMouseEnter
        , Right <$> onMouseLeave
        ]
        []
      case e of
        Left _  -> go True
        Right _ -> go False

--------------------------------------------------------------------------------

counterApp :: IO ()
counterApp = runDefault 3030 "Counter" (\_ -> counter 0)

todosApp :: IO ()
todosApp = runDefault 3030 "Todos" $ \_ -> todos

clientSidePredictionApp :: IO ()
clientSidePredictionApp
  = runDefault 3030 "Client side prediction test" (\_ -> clientSidePrediction "")

mouseEnterLeaveApp :: IO ()
mouseEnterLeaveApp = runDefault 3030 "Mouse enter/leave test" $ \_ -> mouseEnterLeave

--------------------------------------------------------------------------------

dispatch :: UI HTML ()
dispatch = do
 _ <- div []
   [ p [] [ text "Hello" ]
   , button [ onClick ] [ text "Go next" ]
   ]
 c <- div []
   [ p [] [ text "Choose" ]
   , div []
     [ button [ "a" <$ onClick ] [ text "a" ]
     , button [ "b" <$ onClick ] [ text "b" ]
     ]
   ]
 p [] [ text $ "You chose " <> c ]

dispatch2 :: UI HTML a
dispatch2 = do
    _ <- div []
      [ p [] [ text "Hello" ]
      , button [ onClick ] [ text "Go next" ]
      ]
    c <- div []
      [ p [] [ text "Choose" ]
      , button [ "a" <$ onClick ] [ text "a" ]
      , button [ "b" <$ onClick ] [ text "b" ]
      ]
    p [] [ text $ "You chose " <> c ]

--------------------------------------------------------------------------------

anddTest :: IO ()
anddTest = do
  runDefault 3030 "HiLo" $ \_ -> do
    div []
      [ text "TITLE"
      , do
          -- r <- (:) <$> counter 0 <**> ((:) <$> counter 0 <**> fmap pure (counter 0))
          -- text $ "Done: " <> T.pack (show r)
          -- r <- (,,) <$> counter 0 <**> counter 0 <**> counter 0
          -- text $ "Done: " <> T.pack (show r)
          -- andd [counter 0, counter 0, counter 0]
          text $ "Done"
      ]
  where
    counter x
      | x > 5 = do
          div [ onClick ] [ text "Kill" ]
          pure x
      | otherwise = do
          div [ onClick ] [ text (T.pack $ show x) ]
          counter (x + 1)


main :: IO ()
main = todosApp
