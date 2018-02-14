{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, MonadComprehensions #-}
module Optimization where

import Binary
import Machine

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe


type Opt = Machine ()

runOpt m x = evalState (runMaybeT $ runKleisli m x) ()
opt        = machine


-- First of many, otherwise none
oneOf :: (MonadPlus m, Foldable t) => t (m a) -> m a
oneOf = msum

-- Ensure all succeeds
allOf :: (Monad m, Foldable t) => t (m a) -> m ()
allOf = sequence_

vertifyOpt :: Eq a => Machine x a a -> Machine x a a
vertifyOpt o = [ y | x <- yes, y <- o, x /= y ]
