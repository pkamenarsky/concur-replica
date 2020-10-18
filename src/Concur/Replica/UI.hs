{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Concur.Replica.UI
  ( UI
  , Step(..)
  , mapView
  , orr
  -- , (<**>)
  -- , andd
  , view
  , liftSTM
  , liftNonBlockingSTM
  , runUI
  , debugUI
  ) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Concurrent.STM (STM, retry, atomically)
import Control.Monad (ap)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import           Replica.VDOM                 (Attrs, Attr(AText, ABool, AEvent, AMap), HTML, Namespace, VDOM(VNode, VLeaf, VText))

import           Debug.Trace

data Step a = Block { runStep :: STM a } | Step { runStep :: STM a }
  deriving Functor

instance Applicative Step where
  pure a = Step (pure a)
  Block f <*> Block a = Block (f <*> a)
  Step  f <*> Block a = Block (f <*> a)
  Block f <*> Step  a = Block (f <*> a)
  Step  f <*> Step  a = Step  (f <*> a)

instance Monad Step where
  Block m >>= f = Block (m >>= runStep . f)
  Step  m >>= f = Step  (m >>= runStep . f)

instance Alternative Step where
  empty = Block retry
  Block f <|> Block a = Block (f <|> a)
  Step  f <|> Block a = Step  (f <|> a)
  Block f <|> Step  a = Step  (f <|> a)
  Step  f <|> Step  a = Step  (f <|> a)

--------------------------------------------------------------------------------

removeEvents :: VDOM -> VDOM
removeEvents (VNode e attrs ns chs) = VNode e (removeEventsA attrs) ns (fmap removeEvents chs)
removeEvents (VLeaf e attrs ns) = VLeaf e (removeEventsA attrs) ns
removeEvents n = n

removeEventsA :: Attrs -> Attrs
removeEventsA attrs = M.fromList
  [ ( k
    , case v of
        AMap m -> AMap $ removeEventsA m
        _ -> v
    )
  | (k, v) <- M.toList attrs
  , not (isEvent v)
  ]
  where
    isEvent (AEvent _) = True
    isEvent _ = False

data UI v a = UI (Maybe v) (Step (Either a (UI v a)))

carry :: Maybe HTML -> UI HTML a -> UI HTML a
carry v (UI v' ui) = UI (v' <|> fmap (removeEvents <$>) v) ui

mapView :: Monoid u => (u -> v) -> UI u a -> UI v a
mapView f (UI v ui) = UI (Just $ f $ fromMaybe mempty v) (fmap (mapView f) <$> ui)

instance Functor (UI HTML) where
  fmap f (UI v ui) = UI v $ do
    r <- ui
    case r of
      Left a    -> pure $ Left (f a)
      Right ui' -> pure $ Right (f <$> carry v ui')

instance Applicative (UI HTML) where
  pure a = UI Nothing (pure $ Left a)
  (<*>)  = ap

instance Monad (UI HTML) where
  UI v ui >>= f = UI v $ do
    r <- ui
    case r of
      Left a    -> pure $ Right (carry v (f a))
      Right ui' -> pure $ Right (carry v ui' >>= f)

instance Alternative (UI HTML) where
  empty = UI Nothing (Block retry)
  x@(UI fv _) <|> y@(UI av _) = UI (fv <> av) (run x (<|> y) <|> run y (x <|>))
    where
      run (UI v ui) alt = do
        r <- ui
        case r of
          Left a    -> pure $ Left a
          Right ui' -> pure $ Right $ alt (carry v ui')

-- -- | Parallel applicative composition
-- infixl 4 <**>
-- (<**>) :: UI HTML (a -> b) -> UI HTML a -> UI HTML b
-- x@(UI fv _) <**> y@(UI av _) = UI (fv <> av) (run x (<**> y) (<*> y) (\v -> (v <>)) <|> run y (x <**>) (x <*>) (\v -> (<> v)))
--   where
--     run (UI v ui) altui altf altv = do
--       r <- ui
--       case r of
--         Left a    -> pure $ Right $ mapView (altv $ fmap removeEvents $ fromMaybe mempty v) $ altf (pure a)
--         Right ui' -> pure $ Right $ altui (carry v ui')

orr :: [UI HTML a] -> UI HTML a
orr = foldr (<|>) empty

-- andd :: [UI HTML a] -> UI HTML [a]
-- andd = foldr (\a b -> (:) <$> a <**> b) (pure [])

view :: v -> UI v a
view v = UI (Just v) (Block retry)

liftSTM :: STM a -> UI v a
liftSTM stm = UI Nothing (Left <$> Block stm)

liftNonBlockingSTM :: STM a -> UI v a
liftNonBlockingSTM stm = UI Nothing (Left <$> Step stm)

--------------------------------------------------------------------------------

runUI :: UI v a -> (Maybe v, Step (Either a (UI v a)))
runUI (UI v ui) = (v, ui)

debugUI :: Show v => UI v a -> IO a
debugUI (UI v ui) = do
  print v
  r <- atomically (runStep ui)
  case r of
    Left a    -> pure a
    Right ui' -> debugUI ui'
