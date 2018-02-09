{-# LANGUAGE MonadComprehensions, LambdaCase #-}
module Tape where

import Fucktoid
import Optimization

import Control.Monad.State as S
import Data.List.Zipper


type Tape    a   = Zipper a
type Machine a b = State (Tape a) b

runMachine :: Machine a b -> Tape a -> (b, Tape a)
runMachine = runState

runMachine' :: Machine a b -> [a] -> [a]
runMachine' m = toList . snd . runMachine m . fromList

opt :: Opt [a] b -> Machine a (Maybe b)
opt o = runOpt o <$> S.gets (\(Zip l _) -> l)


