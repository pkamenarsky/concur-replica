{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import Concur.Replica
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Monad (forever)
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

route :: forall a b. Route a => (forall x. IO x -> UI HTML x) -> Context -> a -> (a -> UI HTML (AppUpdate a b)) -> UI HTML b
route liftIO Context{call, registerCallback} initial f = do
  chan <- liftIO newHistoryChan
  go initial chan
  where
    newHistoryChan :: IO (TChan String)
    newHistoryChan = do
      chan <- newTChanIO
      cb <- registerCallback $ \path -> atomically (writeTChan chan path)
      call cb "window.onpopstate = function(event) { callCallback(arg, location.pathname); };"
      pure chan

    go :: a -> TChan String -> UI HTML b
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

routingApp :: State -> UI HTML (AppUpdate State ())
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

data LiftIO = forall a. LiftIO (IO a) (a -> IO ())

main :: IO ()
main = do
  iovar <- newEmptyTMVarIO :: IO (TMVar LiftIO)

  _ <- forkIO $ forever $ do
    LiftIO io put <- atomically $ takeTMVar iovar
    forkIO $ io >>= put

  let liftIO io = do
        v <- liftNonBlockingSTM $ do
          v <- newEmptyTMVar
          putTMVar iovar $ LiftIO io (atomically . putTMVar v)
          pure v
        liftSTM $ takeTMVar v

  putStrLn "Starting app"
  runDefault 3030 "Website" $ \ctx -> do
    route liftIO ctx SiteA routingApp
