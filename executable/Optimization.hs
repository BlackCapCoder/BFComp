{-# LANGUAGE MonadComprehensions, FlexibleInstances, MultiParamTypeClasses #-}
module Optimization where

import Binary

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Monad
import Control.Category
import qualified Control.Monad.Fail as Fail
import Data.Function (fix)


newtype Opt a b = Opt { runOpt :: a -> Maybe b }


instance Alternative (Opt a) where
  empty = no
  a <|> b = Opt $ \x -> runOpt a x <|> runOpt b x

instance Category Opt where
  id = yes
  (Opt f) . (Opt g) = Opt $ g >=> f


instance MonadPlus (Opt a) where
  mzero = empty
  mplus = (<|>)

instance Monoid (Opt a b) where
  mempty = empty
  mappend = (<|>)

instance Functor (Opt a) where
  fmap f (Opt o) = Opt $ fmap f . o

instance Applicative (Opt a) where
  pure = Opt . const . Just
  (Opt f) <*> (Opt o) = Opt $ liftA2 ap f o

instance Monad (Opt a) where
  (Opt o) >>= f = Opt $ \x -> o x >>= flip runOpt x . f
  fail = Fail.fail

instance Fail.MonadFail (Opt a) where
  fail _ = mzero

instance Binary Opt where
  yes = Opt Just
  no = Opt $ const Nothing
  (..>) = (>>>)


-- First of many, otherwise none
oneOf :: (MonadPlus m, Foldable t) => t (m a) -> m a
oneOf = msum

-- Ensure all succeeds
allOf :: (Monad m, Foldable t) => t (m a) -> m ()
allOf = sequence_
