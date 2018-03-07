{-# LANGUAGE MonadComprehensions, TypeFamilies, MultiParamTypeClasses #-}
module Optimizations.C where

import Language
import BrainFuck


data C

instance Language C where
  data Op C
    = MemChr

instance Translatable BrainFuck C where
  transOp = [ ([MemChr], xs) | (Loop [Move 1]:xs)<-yes ]

instance Translatable C BrainFuck where
  transOp = [ ([Loop [Move 1]], xs) | (MemChr:xs)<-yes ]
