{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Concur.Core (Widget, orr)
import Concur.Replica (runDefault)
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Text (Text, pack)
import Prelude hiding (div)
import Replica.VDOM (HTML)

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as P
import qualified Concur.Replica.DOM.Props as P
import qualified Network.Wai.Handler.Replica as HR

class Route a where
  fromRoute :: String -> a
  toRoute :: a -> String

route :: forall a b. Route a => HR.Context -> a -> (a -> Widget HTML (Either a b)) -> Widget HTML b
route ctx initial f = do
  chan <- liftIO $ do
    chan <- newChan :: IO (Chan String)
    cb <- HR.registerCallback ctx $ \hash -> writeChan chan hash
    HR.call ctx cb "window.onhashchange = function() { callCallback(arg, location.hash) };"
    pure chan

  go initial chan

  where
    go :: a -> Chan [Char] -> Widget HTML b
    go a chan = do
      r <- orr [ Left <$> f a, Right <$> liftIO (readChan chan) ]
      case r of
        Left (Left a') -> do
          liftIO $ HR.call ctx (toRoute a') "window.location.hash = '#' + arg;"
          go a' chan

        Left (Right b) ->
          pure b

        Right a' ->
          go (fromRoute $ tail a') chan

--------------------------------------------------------------------------------

data State
  = SiteA Int
  | SiteB String
  | SiteC Double

instance Route State where
  toRoute (SiteA a) = "a:" <> show a
  toRoute (SiteB b) = "b:" <> b
  toRoute (SiteC c) = "c:" <> show c

  fromRoute ('a':':':a) = SiteA (read a)
  fromRoute ('b':':':b) = SiteB b
  fromRoute ('c':':':c) = SiteC (read c)
  fromRoute _ = error "Not supported"

routingApp :: State -> Widget HTML (Either State ())
routingApp = \case
  SiteA a -> do
    _ <- H.div [ P.onClick ] [ H.text ("Site A: " <> pack (show a)) ]
    pure $ Left (SiteB "Next")

  SiteB b -> do
    _ <- H.div [ P.onClick ] [ H.text ("Site B: " <> pack (show b)) ]
    pure $ Left (SiteC 66.6)

  SiteC c -> do
    _ <- H.div [ P.onClick ] [ H.text ("Site C: " <> pack (show c)) ]
    pure $ Left (SiteA 666)

main :: IO ()
main =
  runDefault 8080 "Blog" $
    \ctx -> route ctx (SiteA 0) routingApp
