{-# LANGUAGE FlexibleContexts #-}

import Language
import BrainFuck
import Machine
import Optimizations.BrainFuck as B
import Optimizations.Factor    as F
import TapeMachine

import Math.Pogo

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State as S
import Control.Applicative
import Control.Arrow hiding (left, right)
import System.Environment (getArgs)
import qualified Data.List.Zipper as Z
import Data.Word (Word8)
import Debug.Trace
import Data.Array.Unboxed
import Data.Array.Base (unsafeReplace)


main :: IO ()
main = do
  args <- getArgs

  prg <- fmap parse . readFile $
    if null args
       then "../tst/tst.bf"
       else head args

  let prg' = runTape'' Main.optimizer prg

  putStrLn ""
  -- putStrLn $ showProg prg
  putStrLn $ showProg prg'
  -- x <- interpret prg
  -- print x

  -- putStrLn . showProg . runTape' optimizer $ prg
  -- print $ solvePogo 12334351213333122 224 31234234


-- Pop pure ops from to right
popPure :: TapeMachine (Op BrainFuck) () ()
popPure
  = greedy $ do
      left
      guard . isPure =<< cursor
      popr

try' :: TapeMachine (Op BrainFuck) a b -> TapeMachine (Op BrainFuck) a Bool
try' m = do
  x <- yes
  s <- S.get
  let (a,b) = runTape m x s
  case a of
    Nothing -> return False
    Just _ -> S.put b >> return True


initial :: TapeMachine (Op BrainFuck) () ()
initial = do
  a <- try' $ optr' $ greedy unsafePopInit
  end
  b <- try' popPure
  start
  if a && b then yes else no

unsafe :: TapeMachine (Op BrainFuck) () ()
unsafe = greedy . optr' $ oneOf
  [ B.optimize
  , vertifyOpt $ jump F.unsafeOptimize
  ]

safe :: TapeMachine (Op BrainFuck) () ()
safe = greedy $
  optr' $ oneOf
    [ B.optimize
    , vertifyOpt $ jump F.optimize
    ]

safe' = greedy $ safe <|> vertifyOpt (inLoop safe') <|> right

inLoop :: TapeMachine (Op BrainFuck) () a -> TapeMachine (Op BrainFuck) () a
inLoop o = do
  (Loop b) <- cursor
  (Just a, b') <- pure $ runTape' o () b
  S.modify $ Z.replace (Loop b')
  return a


-- Optimization machine
optimizer :: TapeMachine (Op BrainFuck) () ()
optimizer = do
  try initial
  try unsafe
  try safe'
  start

  greedy $ do
    try unsafe
    try . greedy $ safe <|> right
    start >> initial


interpret :: BFProg -> IO (Int, Array Int Word8)
interpret prg
  | mem <- listArray (0, memSize-1) $ repeat 0
  = fmap snd . flip runStateT (0, mem) $ forM_ prg f
  where f = \case
          Move n -> modify $ \(i, a) -> (mod (i+n) memSize, a)
          Add  n -> modify $ \(i, a) -> (i, unsafeReplace a [(i, (a!i)+fromIntegral n)])
          Out -> S.get >>= liftIO . print . fromIntegral . fst
          In  -> liftIO getChar >>= \x -> S.modify $
            \(i,a) -> (i, unsafeReplace a [(i, fromIntegral $ fromEnum x)])
          Loop b -> whileM_ ((/=0) <$> S.gets (\(i,a) -> a!i)) $ forM_ b f
          Inf -> error "Tried to interpret an infinite loop"

