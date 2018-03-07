{-# LANGUAGE FlexibleContexts #-}
module Optimization
  ( module Optimization
  , module Binary
  ) where

import Binary
import Machine

import Control.Applicative (Alternative)
import Control.Arrow (runKleisli, first)
import Control.Monad.State (evalState)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Foldable (asum, sequenceA_, traverse_)
import Data.Function (fix)
import Data.Maybe (maybe, isJust)


type Opt = Machine ()

runOpt m x = evalState (runMaybeT $ runKleisli m x) ()
opt        = machine

-- First of many, otherwise none
oneOf :: (Alternative m, Foldable t) => t (m a) -> m a
oneOf = asum

-- Ensure all succeeds
allOf :: (Applicative m, Foldable t) => t (m a) -> m ()
allOf = sequenceA_

-- Try all in order
tryAll :: (Binary f b, Traversable t) => t (f b b) -> f b ()
tryAll = traverse_ try

-- Use Eq to vertify that an optimization has really changed anything TODO
vertifyOpt :: Eq a => Machine x a a -> Machine x a a
vertifyOpt o = [ y | x <- yes, y <- o, x /= y ]


exec :: Opt a b -> a -> Opt c b
exec o x = [ q | Just q <- pure $ runOpt o x ]

spanOpt :: Monoid b => Opt a b -> Opt [a] (b, [a])
spanOpt o = do
  (a:as) <- yes
  b <- exec o a
  return . maybe (b, as) (first $ mappend b) $ runOpt (spanOpt o) as

