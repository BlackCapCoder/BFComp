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


tst :: POpt BrainFuck
tst = do
  (Memchr, xs) <- get
  Just (Memchr, ys) <- pure $ runOpt get xs
  return $ put Memchr ++ ys

tst' :: POpt C
tst' = [ Memchr:xs | (Memchr:Memchr:xs) <- id ]


