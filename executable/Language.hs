{-# Language TupleSections, FlexibleContexts #-}
module Language
  ( module Language
  , module Optimization
  )
  where

import Optimization

data family Op l :: *

type Program l = [Op l]
type POpt    l = Opt (Program l) (Program l)

class Translatable a b where
  transOp :: Opt (Op a) (Program b)
  transOp = fmap fst $ pure <$> yes >>> transOps

  -- gets
  transOps :: Opt (Program a) (Program b, Program a)
  transOps = spanOpt transOp

  trans :: Opt (Program a) (Program b)
  trans = fst <$> transOps

instance Translatable a a where
  trans    = yes
  transOps = (, []) <$> trans


liftL :: Translatable a b => Opt (Program b) c -> Opt (Program a) c
liftL o = trans >>> o

-- Run an optimization for language A in language B
jump :: (Translatable a b, Translatable b c) => Opt (Program c) (Program a) -> Opt (Program b) (Program b)
jump o = [ x ++ b | (a, b) <- transOps, x <- exec (o >>> trans) a ]


-----

data TC       l
type TOp      l = Op (TC l)
type TProgram l = [TOp l]
type TPOpt    l = Opt (TProgram l) (TProgram l)

-- A turing-complete language is a language that might diverge
data instance Op (TC l)
  = Total (Op l) | Diverge

unTC :: Opt (TOp l) (Op l)
unTC = [ x | Total x <- yes ]

-- Translating a language does not change totality
instance Translatable a b => Translatable (TC a) (TC b) where
  transOp = oneOf
    [ [ Total <$> bs | Total a <- yes, bs <- exec transOp a ]
    , [ pure Diverge | Diverge <- yes ]
    ]

-- In practice totality is undecidable, so this function
-- cannot return a true answer 100% of the time, as this
-- would solve the halting problem
isTotal :: TOp l -> Bool
isTotal = \case Diverge -> False; _ -> True

instance (Show (Op l)) => Show (TOp l) where
  show Diverge   = "(infinite loop)"
  show (Total x) = show x

