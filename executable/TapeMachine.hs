{-# LANGUAGE MonadComprehensions, LambdaCase, TypeSynonymInstances #-}
module TapeMachine where

import Binary
import Machine
import Optimization

import Control.Arrow hiding (left, right)
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List.Zipper (Zipper (..), toList, fromList)
import qualified Data.List.Zipper as Z


type Tape          = Zipper
type TapeMachine s = Machine (Tape s)


runTape = Machine.runMachine

runTape' m a s
  | (x,y) <- runTape m a $ fromList s
  = (x, toList y)

runTape'' :: TapeMachine a () b -> [a] -> [a]
runTape'' m = snd . runTape' m ()

-- Somehow the list zipper doesn't ship with these
zLeft  = \case Zip l r -> l
zRight = \case Zip l r -> r

start, end, pop, popr, left, right :: TapeMachine a b ()
start = modify Z.start -- == greedy left
end   = modify Z.end   -- == greedy right
pop   = modify Z.pop
popr  = right >> modify Z.pop

-- Move the tape left
left = do
  guard . not . null =<< gets zLeft
  modify Z.left

-- Move the tape right
right = do
  guard . not . null =<< gets zRight
  modify Z.right

tapeStart :: TapeMachine a b Bool
tapeStart = gets Z.beginp

tapeEnd :: TapeMachine a b Bool
tapeEnd = gets Z.endp


-- Gets the cursor
cursor :: TapeMachine a b a
cursor = machine . const . MaybeT $ gets Z.safeCursor



-- Run an optimization with the left tape
optl :: Opt [a] b -> TapeMachine a c (Maybe b)
optl o = runOpt o <$> gets zLeft

-- Run an optimization with the right tape
optr :: Opt [a] b -> TapeMachine a c (Maybe b)
optr o = runOpt o <$> gets zRight

-- Run an optimization on the left tape
optl' :: Opt [a] [a] -> TapeMachine a c ()
optl' o = do
  Just x <- optl o
  modify $ \(Zip l r) -> Zip x r

-- Run an optimization on the right tape
optr' :: Opt [a] [a] -> TapeMachine a c ()
optr' o = do
  Just x <- optr o
  modify $ \(Zip l r) -> Zip l x


