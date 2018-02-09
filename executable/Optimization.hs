{-# LANGUAGE MonadComprehensions #-}
module Optimization where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Monad
import Control.Category
import qualified Control.Monad.Fail as Fail
import Data.Function (fix)


newtype Opt a b = Opt { runOpt :: a -> Maybe b }

yes :: Opt a a
yes = Opt Just

no :: Opt a b
no = Opt $ const Nothing


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


-- First of many, otherwise none
oneOf :: Foldable t => t (Opt a b) -> Opt a b
oneOf = msum

-- Ensure all succeeds
allOf :: Traversable t => t (Opt a b) -> Opt a ()
allOf = sequence_

-- An optimization that cannot fail
try :: (Category f, Alternative (f a)) => f a a -> f a a
try = (<|> id)

-- Optional optimization appliance
(.>) :: (Category f, Alternative (f b)) => f a b -> f b b -> f a b
a .> b = a >>> try b

-- Repeat as many times as possible, but at least once
greedy :: (Category f, Alternative (f a)) => f a a -> f a a
greedy = fix $ ap (.>)


{-# RULES
"doubleGreedy" forall x. greedy (greedy x) = greedy x
"doubleTry" forall x. try (try x) = try x
  #-}
