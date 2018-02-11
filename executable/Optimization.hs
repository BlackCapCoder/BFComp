{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Optimization where

import Binary

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Monad
import Control.Applicative
import Control.Category
import qualified Control.Monad.Fail as Fail


type Opt = Kleisli Maybe

runOpt = runKleisli
wrap   = Kleisli


instance Functor (Opt a) where
  fmap f o = wrap $ fmap f . runOpt o

instance Applicative (Opt a) where
  pure = wrap . const . Just
  f <*> o = wrap $ liftA2 ap (runOpt f) (runOpt o)

instance Monad (Opt a) where
  o >>= f = wrap $ \x -> runOpt o x >>= flip runOpt x . f
  fail = Fail.fail

instance Fail.MonadFail (Opt a) where
  fail _ = no

instance Alternative (Opt a) where
  empty = wrap $ const Nothing
  a <|> b = wrap $ \x -> runOpt a x <|> runOpt b x

instance MonadPlus (Opt a) where
  mzero = no
  mplus = (<|>)

instance Binary Opt where
  yes = id
  no  = empty
  (..>) = (>>>)


-- First of many, otherwise none
oneOf :: (MonadPlus m, Foldable t) => t (m a) -> m a
oneOf = msum

-- Ensure all succeeds
allOf :: (Monad m, Foldable t) => t (m a) -> m ()
allOf = sequence_
