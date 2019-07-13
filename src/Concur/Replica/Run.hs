module Concur.Replica.Run
  ( run
  , runDefault
  ) where

import           Concur.Core                     (SuspendF(StepView, StepIO, StepBlock, Forever), Widget, display, orr, step)

import           Control.Monad.Free              (Free(Pure, Free))

import qualified Data.ByteString.Lazy            as B
import qualified Data.ByteString.Char8           as BC
import           Data.Aeson                      ((.:), (.=))
import qualified Data.Aeson                      as A
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe, mapMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Unsafe.Coerce                   (unsafeCoerce)

import qualified Replica.VDOM                    as V
import           Replica.VDOM                    (fireEvent, defaultIndex)
import           Replica.VDOM.Types              (DOMEvent(DOMEvent), HTML)

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import qualified Network.Wai.Handler.Replica     as R
import qualified Network.Wai.Handler.Warp        as W

import           Debug.Trace

stepWidget :: Free (SuspendF HTML) a -> IO (Maybe (HTML, (Free (SuspendF HTML) a), R.Event -> Maybe (IO ())))
stepWidget v = case v of
  Pure a                   -> pure Nothing
  Free (StepView new next) -> pure $ Just (new, next, \event -> fireEvent new (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event))
  Free (StepIO io next)    -> io >>= stepWidget . next
  Free (StepBlock io next) -> io >>= stepWidget . next
  Free Forever             -> pure Nothing

run :: Int -> HTML -> ConnectionOptions -> Widget HTML a -> IO ()
run port index connectionOptions widget
  = W.run port
  $ R.app index connectionOptions (step widget) stepWidget

runDefault :: Int -> T.Text -> Widget HTML a -> IO ()
runDefault port title widget
  = W.run port
  $ R.app (defaultIndex title []) defaultConnectionOptions (step widget) stepWidget
