module Binary where

import Control.Applicative
import Data.Function
import Control.Monad


class Binary f where
  no  :: f a b
  yes :: f a a
  (..>) :: f a b -> f b c -> f a c

  yes' :: f a ()
  (~.>) :: f a b -> f a c -> f a c


try :: (Binary f, Alternative (f a)) => f a a -> f a a
try = (<|> yes)

try' :: (Binary f, Alternative (f a)) => f a () -> f a ()
try' = (<|> yes')

(.>) :: (Binary f, Alternative (f c)) => f a c -> f c c -> f a c
a .> b = a ..> try b

(~>) :: (Binary f, Alternative (f a)) => f a b -> f a () -> f a ()
a ~> b = a ~.> try' b

greedy :: (Binary f, Alternative (f a)) => f a a -> f a a
greedy = fix $ ap (.>)

greedy' :: (Binary f, Alternative (f a)) => f a () -> f a ()
greedy' = fix $ ap (~>)


{-# RULES
"doubleGreedy" forall x. greedy (greedy x) = greedy x
"doubleTry" forall x. try (try x) = try x
  #-}
