module Optimizations.C where

import Language
import BrainFuck


data C

data instance Op C
  = MemChr


instance Translatable BrainFuck C where
  transOp = [ pure $ MemChr | Loop [Move 1]<-yes ]

instance Translatable C BrainFuck where
  transOp = [ pure $ Loop [Move 1] | MemChr<-yes ]
