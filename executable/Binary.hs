{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Binary where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Function (fix)
import Prelude hiding (id)

class (Category f, Alternative (f a)) => Binary f a where
  yes :: f a a
  yes = id

  no :: f a b
  no = empty


try :: Binary f a => f a a -> f a a
try = (<|> yes)

(.>) :: Binary f c => f a c -> f c c -> f a c
a .> b = a >>> try b

greedy :: Binary f a => f a a -> f a a
greedy = fix $ ap (.>)

