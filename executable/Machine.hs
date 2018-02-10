{-# LANGUAGE MonadComprehensions, LambdaCase, TypeSynonymInstances, MultiParamTypeClasses #-}
module Machine where

import Optimization hiding (get, gets, yes, no, try, (.>))
import Binary

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.List.Zipper as Z
import Control.Applicative

type Tape = Zipper
newtype Machine a b = Machine { unMachine :: MaybeT (State (Tape a)) b }


instance Functor (Machine a) where
  fmap f (Machine x) = Machine $ f <$> x

instance Applicative (Machine a) where
  pure = Machine . pure
  (Machine f) <*> (Machine x) = Machine $ f <*> x

instance Monad (Machine a) where
  (Machine a) >>= f = Machine $ a >>= unMachine <$> f

instance MonadPlus (Machine a) where
  mzero = Machine mzero
  (Machine a) `mplus` (Machine b) = Machine $ a `mplus` b

instance Alternative (Machine a) where
  empty = mzero
  (Machine a) <|> (Machine b) = Machine $ a <|> b

instance MonadState (Tape a) (Machine a) where
  get   = Machine $ get
  put   = Machine . put
  state = Machine . state

instance Binary Machine where
  no = mzero
  yes' = pure ()
  a ~.> b = a >> b


-- Somehow the list zipper doesn't ship with these
zLeft  = \case Zip l r -> l
zRight = \case Zip l r -> r

-- Wrap the zipper in our monad
start, end, pop, popr, left, right :: Machine a ()
start = modify Z.start -- == greedy left
end   = modify Z.end   -- == greedy right
pop   = modify Z.pop
popr  = Machine.right >> modify Z.pop

-- Move the tape left
left = do
  guard . not . null =<< gets zLeft
  modify Z.left

-- Move the tape right
right = do
  guard . not . null =<< gets zRight
  modify Z.right

-- Gets the cursor
cursor :: Machine a a
cursor = Machine . MaybeT $ gets Z.safeCursor



-- Run an optimization with the left tape
optl :: Opt [a] b -> Machine a (Maybe b)
optl o = runOpt o <$> gets zLeft

-- Run an optimization with the right tape
optr :: Opt [a] b -> Machine a (Maybe b)
optr o = runOpt o <$> gets zRight

-- Run an optimization on the left tape
optl' :: Opt [a] [a] -> Machine a ()
optl' o = do
  Just x <- optl o
  modify $ \(Zip l r) -> Zip x r

-- Run an optimization on the right tape
optr' :: Opt [a] [a] -> Machine a ()
optr' o = do
  Just x <- optr o
  modify $ \(Zip l r) -> Zip l x



runMachine :: Machine a b -> Tape a -> (Maybe b, Tape a)
runMachine = runState . runMaybeT . unMachine

runMachine' :: Machine a b -> [a] -> [a]
runMachine' m = toList . snd . runMachine m . fromList
