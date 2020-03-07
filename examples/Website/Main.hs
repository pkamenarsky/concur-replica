{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mimic a static site with concur replica.
--
-- WIP, but will eventually have URL path based routing
-- and server-rendered initial pages.
module Main where

import Concur.Core (Widget, orr)
import Concur.Replica (runDefault)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Prelude
import Replica.VDOM (HTML)

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as P
import qualified Network.Wai.Handler.Replica as HR

class Route a where
  fromRoute :: String -> a
  toRoute :: a -> String

data AppUpdate a b
  = UpdateChangeUrl a
  | UpdateExit b

route :: forall a b. Route a => HR.Context -> a -> (a -> Widget HTML (AppUpdate a b)) -> Widget HTML b
route ctx initial f = do
  chan <- liftIO newHistoryChan
  go initial chan
  where
    newHistoryChan :: IO (Chan String)
    newHistoryChan = do
      chan <- newChan
      forkIO $ logChan =<< dupChan chan
      cb <- HR.registerCallback ctx $ \path -> writeChan chan path
      HR.call ctx cb "window.onpopstate = function(event) { callCallback(arg, location.pathname); };"
      pure chan

    logChan :: Chan String -> IO ()
    logChan chan = do
      e <- readChan chan
      putStrLn e
      logChan chan

    -- go :: a -> Chan String -> Widget HTML b
    go a chan = do
      res <- liftIO $ async (readChan chan)
      r <- orr [ Left <$> f a, Right <$> liftIO (wait res) ]
      case r of
        Left (UpdateChangeUrl a') -> do
          -- liftIO $ putStrLn "pushing state"
          liftIO $ HR.call ctx (toRoute a') "window.history.pushState({}, \"\", arg);"
          liftIO $ cancel res
          go a' chan

        Left (UpdateExit b) -> do
          liftIO $ cancel res
          pure b

        Right path ->
          -- liftIO (putStrLn "popstate callback received") *>
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
    "/" ->
      SiteA

    '/':'b':'-':b ->
      SiteB b

    '/':'c':'-':c ->
      SiteC (read c)

    _ ->
      error "Invalid URL"

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
  putStrLn "Starting"
  runDefault 8080 "Website" $
    \ctx -> liftIO (putStrLn "===== thread begins") *> route ctx SiteA routingApp
