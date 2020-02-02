module Concur.Replica.Run
  ( run
  , runDefault
  ) where

import           Concur.Core                     (SuspendF(StepView, StepIO, StepBlock, Forever), Widget, step)

import           Control.Monad.Free              (Free(Pure, Free))

import qualified Data.Text                       as T

import           Replica.VDOM                    (fireEvent, defaultIndex)
import           Replica.VDOM.Types              (DOMEvent(DOMEvent), HTML)

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai                     (Middleware)
import qualified Network.Wai.Handler.Replica     as R
import qualified Network.Wai.Handler.Warp        as W

stepWidget :: Free (SuspendF HTML) a -> IO (Maybe (HTML, (Free (SuspendF HTML) a), R.Event -> Maybe (IO ())))
stepWidget v = case v of
  Pure _                   -> pure Nothing
  Free (StepView new next) -> pure $ Just (new, next, \event -> fireEvent new (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event))
  Free (StepIO io next)    -> io >>= stepWidget . next
  Free (StepBlock io next) -> io >>= stepWidget . next
  Free Forever             -> pure Nothing

run :: Int -> HTML -> ConnectionOptions -> Middleware -> Widget HTML a -> IO ()
run port index connectionOptions middleware widget
  = W.run port
  $ R.app index connectionOptions middleware (step widget) stepWidget

runDefault :: Int -> T.Text -> Widget HTML a -> IO ()
runDefault port title widget
  = W.run port
  $ R.app (defaultIndex title []) defaultConnectionOptions id (step widget) stepWidget
