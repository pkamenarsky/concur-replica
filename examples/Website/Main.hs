{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mimic a static site with concur replica.
--
-- Currently updates the URL as you move between pages,
-- which allows you to use the back button.
--
-- WIP, and will eventually support:
--
-- + Going directly to pages by URL
-- + Doing the rendering for initial pages on the server
module Main where

import Concur.Core (Widget, liftSTM, orr)
import Concur.Replica (runDefault)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Network.Wai.Handler.Replica (Context(Context, call, registerCallback))
import Prelude
import Replica.VDOM (HTML)

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as P

class Route a where
  fromRoute :: String -> a
  toRoute :: a -> String

data AppUpdate a b
  = UpdateChangeUrl a
  | UpdateExit b

route :: forall a b. Route a => Context -> a -> (a -> Widget HTML (AppUpdate a b)) -> Widget HTML b
route Context{call, registerCallback} initial f = do
  chan <- liftIO newHistoryChan
  go initial chan
  where
    newHistoryChan :: IO (TChan String)
    newHistoryChan = do
      chan <- newTChanIO
      cb <- registerCallback $ \path -> atomically (writeTChan chan path)
      call cb "window.onpopstate = function(event) { callCallback(arg, location.pathname); };"
      pure chan

    go :: a -> TChan String -> Widget HTML b
    go a chan = do
      r <- orr [ Left <$> f a, Right <$> liftSTM (readTChan chan) ]
      case r of
        Left (UpdateChangeUrl a') -> do
          liftIO $ call (toRoute a') "window.history.pushState({}, \"\", arg);"
          go a' chan

        Left (UpdateExit b) ->
          pure b

        Right path ->
          go (fromRoute path) chan

--------------------------------------------------------------------------------

data State
  = SiteA
  | SiteB String
  | SiteC Double

instance Route State where
  toRoute = \case
    SiteA   -> "/"
    SiteB b -> "/b-" <> b
    SiteC c -> "/c-" <> show c

  fromRoute = \case
    "/"           -> SiteA
    '/':'b':'-':b -> SiteB b
    '/':'c':'-':c -> SiteC (read c)
    _             -> error "Invalid URL"

routingApp :: State -> Widget HTML (AppUpdate State ())
routingApp = \case
  SiteA -> do
    _ <- H.div [ P.onClick ] [ H.text ("Site A (click for next site)") ]
    pure $ UpdateChangeUrl (SiteB "Next")

  SiteB b -> do
    _ <- H.div [ P.onClick ] [ H.text ("Site B: " <> pack (show b)) ]
    pure $ UpdateChangeUrl (SiteC 66.6)

  SiteC c -> do
    _ <- H.div [ P.onClick ] [ H.text ("Site C: " <> pack (show c)) ]
    pure $ UpdateChangeUrl SiteA

main :: IO ()
main = do
  putStrLn "Starting app"
  runDefault 8080 "Website" $
    \ctx -> do
      liftIO (putStrLn "Client connected")
      route ctx SiteA routingApp
