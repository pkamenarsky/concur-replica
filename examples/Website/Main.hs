{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mimic a static site with concur replica.
--
-- WIP, but will eventually have URL path based routing
-- and server-rendered initial pages.
module Main where

import Control.Concurrent

import Concur.Core (Widget, orr)
import Concur.Replica (runDefault)
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
      cb <- HR.registerCallback ctx $ \path -> writeChan chan path
      HR.call ctx cb "window.onpopstate = function() { callCallback(arg, location.pathname); console.log(\"popstate callback complete \" + location.pathname); };"
      pure chan

    go :: a -> Chan String -> Widget HTML b
    go a chan = do
      r <- orr [ Left <$> f a, Right <$> liftIO (readChan chan) ]
      case r of
        Left (UpdateChangeUrl a') -> do
          -- liftIO $ threadDelay 500000
          liftIO $ putStrLn "pushing state"
          liftIO $ HR.call ctx (toRoute a') "window.history.pushState(null, \"\", arg);"
          go a' chan

        Left (UpdateExit b) ->
          pure b

        Right path ->
          liftIO (putStrLn "popstate callback received") *>
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
      SiteA
      -- error "Invalid URL"

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
main =
  runDefault 8080 "Website" $
    \ctx -> route ctx SiteA routingApp
