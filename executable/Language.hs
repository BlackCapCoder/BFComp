{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, MonadComprehensions #-}
module Language
  ( Language (..)
  , Translatable (..)
  , Program, POpt
  , op
  , liftL
  , jump

  , module Binary
  , module Optimization
  )
  where

import Binary
import Optimization
import Control.Category ((>>>))


class Language l where
  data Op l :: *

type Program l = [Op l]
type POpt l = Opt (Program l) (Program l)

class Translatable a b where
  transOp :: Opt (Program a) (Program b, Program a)
  transOp = [ (bs, []) | bs <- trans ]

  -- gets
  transOps :: Opt (Program a) (Program b, Program a)
  transOps = [ (concat bs, last as) | (bs, as) <- scanOpt transOp ]

  trans :: Opt (Program a) (Program b)
  trans = fst <$> transOps

instance Translatable a a where
  trans = yes


op :: Opt (Op a) (Op b) -> Opt (Program a) (Program b, Program a)
op o = [ ([x'], xs) | (x:xs)<-yes, Just x' <- pure $ runOpt o x ]

liftL :: Translatable a b => Opt (Program b) c -> Opt (Program a) c
liftL o = [ x | Just x <- runOpt o <$> trans ]

-- Run an optimization for language A in language B
jump :: (Translatable a b, Translatable b c) => Opt (Program c) (Program a) -> Opt (Program b) (Program b)
jump o = [ x ++ b | (a, b) <- transOps, x <- exec (o >>> trans) a ]
