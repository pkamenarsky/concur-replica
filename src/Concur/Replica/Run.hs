module Concur.Replica.Run where

import           Concur.Core                     (SuspendF(StepView, StepIO, StepBlock, StepSTM, Forever), Widget, step)

import           Control.Monad.Free              (Free(Pure, Free))
import           Control.Concurrent.STM          (atomically)

import qualified Data.Text                       as T

import           Replica.VDOM                    (fireEvent, defaultIndex)
import           Replica.VDOM.Types              (DOMEvent(DOMEvent), HTML)

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai                     (Middleware)
import qualified Network.Wai.Handler.Replica     as R
import qualified Network.Wai.Handler.Warp        as W

run :: Int -> HTML -> ConnectionOptions -> Middleware -> (R.Context -> Widget HTML a) -> IO ()
run port index connectionOptions middleware widget
  = W.run port
  $ R.app index connectionOptions middleware (step <$> widget) stepWidget

runDefault :: Int -> T.Text -> (R.Context -> Widget HTML a) -> IO ()
runDefault port title widget
  = W.run port
  $ R.app (defaultIndex title []) defaultConnectionOptions id (step <$> widget) stepWidget

-- | No need to use this directly if you're using 'run' or 'runDefault'.
stepWidget :: R.Context -> (R.Context -> Free (SuspendF HTML) a) -> IO (Maybe (HTML, R.Context -> Free (SuspendF HTML) a, R.Event -> Maybe (IO ())))
stepWidget ctx v = case v ctx of
  Pure _                   -> pure Nothing
  Free (StepView new next) -> pure $ Just (new, const next, \event -> fireEvent new (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event))
  Free (StepIO io next)    -> io >>= stepWidget ctx . \r _ -> next r
  Free (StepBlock io next) -> io >>= stepWidget ctx . \r _ -> next r
  Free (StepSTM stm next)  -> atomically stm >>= stepWidget ctx . \r _ -> next r
  Free Forever             -> pure Nothing
