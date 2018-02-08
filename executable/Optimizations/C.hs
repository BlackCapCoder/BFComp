{-# LANGUAGE MonadComprehensions, TypeFamilies #-}
module Optimizations.C where

import Fucktoid
import Optimization

import Prelude hiding ((.), id)
import Control.Category


data C

instance Fucktoid C where
  data Op C
    = Memchr

  put Memchr = [Loop [Move 1]]
  get = [(Memchr, xs) | (Loop [Move 1]:xs) <- id]

