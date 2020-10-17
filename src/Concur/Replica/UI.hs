{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Concur.Replica.UI
  ( UI
  , Step(..)
  , mapView
  -- , (<**>)
  , orr
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

import Data.Maybe (fromMaybe)

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

data UI v a = UI (Maybe v) (Step (Either a (UI v a)))

carry :: Maybe v -> UI v a -> UI v a
carry v (UI v' ui) = UI (v' <|> v) ui

mapView :: Monoid u => (u -> v) -> UI u a -> UI v a
mapView f (UI v ui) = UI (Just $ f $ fromMaybe mempty v) (fmap (mapView f) <$> ui)

instance Functor (UI v) where
  fmap f (UI v ui) = UI v $ do
    r <- ui
    case r of
      Left a    -> pure $ Left (f a)
      Right ui' -> pure $ Right (f <$> carry v ui')

instance Monoid v => Applicative (UI v) where
  pure a = UI mempty (pure $ Left a)
  (<*>)  = ap

instance Monoid v => Monad (UI v) where
  UI v ui >>= f = UI v $ do
    r <- ui
    case r of
      Left a    -> pure $ Right (carry v (f a))
      Right ui' -> pure $ Right (carry v ui' >>= f)

instance Monoid v => Alternative (UI v) where
  empty = UI mempty (Block retry)
  x@(UI fv _) <|> y@(UI av _) = UI (fv <> av) (run x (<|> y) <|> run y (x <|>))
    where
      run (UI v ui) alt = do
        r <- ui
        case r of
          Left a    -> pure $ Left a
          Right ui' -> pure $ Right $ alt (carry v ui')

-- -- | Parallel applicative composition
-- infixl 4 <**>
-- (<**>) :: Monoid v => UI v (a -> b) -> UI v a -> UI v b
-- x@(UI fv _) <**> y@(UI av _) = UI (fv <> av) (run x (<**> y) (<*> y) <|> run y (x <**>) (x <*>))
--   where
--     run (UI v ui) alt parap = do
--       r <- ui
--       case r of
--         Left a    -> pure $ Right $ parap (pure a)
--         Right ui' -> pure $ Right $ alt (carry v ui')

orr :: Monoid v => [UI v a] -> UI v a
orr = foldr (<|>) empty

-- andd :: Monoid v => [UI v a] -> UI v [a]
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
