module Optimization where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Category


newtype Opt a b = Opt { runOpt :: a -> Maybe b }

yes :: Opt a a
yes = Opt Just

no :: Opt a b
no = Opt $ const Nothing


instance Alternative (Opt a) where
  empty = no
  a <|> b = Opt $ \x -> runOpt a x <|> runOpt b x

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
  (Opt f) <*> (Opt o) = Opt $ \x -> f x <*> o x

instance Monad (Opt a) where
  (Opt o) >>= f = Opt $ \x ->
    o x >>= \a -> f a `runOpt` x
  fail = Fail.fail

instance Fail.MonadFail (Opt a) where
  fail _ = mzero

instance Category Opt where
  (Opt f) . (Opt g) = Opt $ g >=> f
  id = yes


-- First of many, otherwise none
oneOf :: Foldable t => t (Opt a b) -> Opt a b
oneOf = msum

-- Ensure all succeeds
allOf :: Traversable t => t (Opt a b) -> Opt a ()
allOf = sequence_

-- An optimization that cannot fail
try :: Opt a a -> Opt a a
try = (<|> id)

-- Repeat as many times as possible, but at least once
greedy :: Opt a a -> Opt a a
greedy o@(Opt f) = Opt $ \x -> f x >>= runOpt (try $ greedy o)

-- Optional optimization appliance
(.>) :: Opt a b -> Opt b b -> Opt a b
a .> b = a >>> try b
