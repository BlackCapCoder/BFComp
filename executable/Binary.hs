{-# LANGUAGE MultiParamTypeClasses #-}
module Binary where

import Prelude hiding (id)
import Control.Applicative
import Data.Function (fix)
import Control.Monad
import Control.Category

class (Category f, Alternative (f a)) => Binary f a where
  yes :: f a a
  yes = id

  no :: f a b
  no = empty

  (..>) :: f a b -> f b c -> f a c
  (..>) = (>>>)


try :: Binary f a => f a a -> f a a
try = (<|> yes)

(.>) :: Binary f c => f a c -> f c c -> f a c
a .> b = a >>> try b

greedy :: Binary f a => f a a -> f a a
greedy = fix $ ap (.>)

{-# RULES
"doubleGreedy" forall x. greedy (greedy x) = greedy x
"doubleTry" forall x. try (try x) = try x
  #-}
