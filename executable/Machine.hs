{-# LANGUAGE MonadComprehensions, LambdaCase #-}
module Machine where

import Optimization hiding (get, gets)

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.List.Zipper as Z
import Control.Applicative

type Tape      = Zipper
type Machine a = MaybeT (State (Tape a))



-- Somehow the list zipper doesn't ship with these
zLeft  = \case Zip l r -> l
zRight = \case Zip l r -> r

-- Wrap the zipper in our monad
start, end, pop, popr, left, right :: Machine a ()
start = modify Z.start -- == mgreedy left
end   = modify Z.end -- == mgreedy right
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
cursor = MaybeT $ gets Z.safeCursor



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

-- These might come in handy
(<<.) = optl'
(.>>) = optr'



runMachine :: Machine a b -> Tape a -> (Maybe b, Tape a)
runMachine = runState . runMaybeT

runMachine' :: Machine a b -> [a] -> [a]
runMachine' m = toList . snd . runMachine m . fromList

-- Machines can be greedy too!
mgreedy :: Machine a () -> Machine a ()
mgreedy m = do
  (Just (), t) <- runMachine m <$> get
  let (_, tb) = runMachine (mgreedy m) t
  modify $ const tb

mtry :: Machine a () -> Machine a ()
mtry = (<|> pure ())

