{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.Chan
import Concur.Replica
import Control.Monad (forever)
import Data.Maybe
import Data.Text (Text, pack)
import Replica.VDOM (HTML)

import Network.Wai.Handler.Replica as R

import Prelude hiding (div)

class Route a where
  fromRoute :: String -> a
  toRoute :: a -> String

route :: Route a => (forall x. IO x -> UI HTML x) -> R.Context -> a -> (a -> UI HTML (Either a b)) -> UI HTML b
route liftIO ctx a f = do
  chan <- liftIO $ do
    chan <- newChan :: IO (Chan String)
    cb <- R.registerCallback ctx $ \hash -> writeChan chan hash
    R.call ctx cb "window.onhashchange = function() { callCallback(arg, location.hash) };"
    pure chan

  go a chan

  where
    go a chan = do
      r <- orr [ Left <$> f a, Right <$> liftIO (readChan chan) ]
      case r of
        Left (Left a') -> do
          liftIO $ R.call ctx (toRoute a') "window.location.hash = '#' + arg;"
          go a' chan
        Left (Right b) -> pure b
        Right a' -> go (fromRoute $ tail a') chan

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

routingApp :: State -> UI HTML (Either State ())
routingApp (SiteA a) = do
  div [ onClick ] [ text ("Site A: " <> pack (show a)) ]
  pure $ Left (SiteB "Next")
routingApp (SiteB b) = do
  div [ onClick ] [ text ("Site B: " <> pack (show b)) ]
  pure $ Left (SiteC 66.6)
routingApp (SiteC c) = do
  div [ onClick ] [ text ("Site C: " <> pack (show c)) ]
  pure $ Left (SiteA 666)

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

  runDefault 3030 "Select" $ \ctx -> route liftIO ctx (SiteA 0) routingApp
