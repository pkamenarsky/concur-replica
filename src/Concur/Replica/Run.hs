{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Concur.Replica.Run where

import           Control.Concurrent.STM          (atomically)

import qualified Data.Text                       as T
import           Data.Maybe                      (fromMaybe)

import           Concur.Replica.UI               (UI, Step(Block, Step), runUI)

import           Replica.VDOM                    (fireEvent, defaultIndex)
import           Replica.VDOM.Types              (DOMEvent(DOMEvent), HTML)

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai                     (Middleware)
import qualified Network.Wai.Handler.Replica     as R
import qualified Network.Wai.Handler.Warp        as W


run :: Int -> HTML -> ConnectionOptions -> Middleware -> (R.Context -> UI HTML a) -> IO ()
run port index connectionOptions middleware ui
  = W.run port
  $ R.app index connectionOptions middleware ui stepUI

runDefault :: Int -> T.Text -> (R.Context -> UI HTML a) -> IO ()
runDefault port title ui
  = W.run port
  $ R.app (defaultIndex title []) defaultConnectionOptions id ui stepUI

-- | No need to use this directly if you're using 'run' or 'runDefault'.
stepUI :: R.Context -> (R.Context -> UI HTML a) -> (IO (HTML, R.Event -> Maybe (IO ()), IO (Maybe (R.Context -> UI HTML a))))
stepUI ctx mkUI = go (mkUI ctx)
  where
    go v = do
      case ui of
        Step stm -> do
          r <- atomically stm
          case r of
            Left _   -> pure (html, \_ -> Nothing, pure Nothing)
            Right v' -> go v'

        Block stm -> do
          pure
            ( html
            , \event -> fireEvent html (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event) 
            , do
                r <- atomically stm
                case r of
                  Left _    -> pure Nothing
                  Right ui' -> pure $ Just (const ui')
            )

      where
        (html', ui) = runUI v
        html = fromMaybe mempty html'
