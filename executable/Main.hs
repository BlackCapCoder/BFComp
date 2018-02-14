{-# LANGUAGE MonadComprehensions, LambdaCase, FlexibleContexts #-}

import Binary
import Fucktoid
import Optimization
import Machine
import Optimizations.BrainFuck as B
import Optimizations.Factor    as F
import TapeMachine

import Pogo

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State as S
import Control.Applicative
import Control.Arrow hiding (left, right)
import System.Environment (getArgs)
import qualified Data.List.Zipper as Z
import Data.Word (Word8)
import Debug.Trace


main :: IO ()
main = do
  args <- getArgs

  prg <- fmap parse . readFile $
    if null args
       then "tst/tst3.bf"
       else head args

  let prg' = runTape'' Main.optimizer prg

  putStrLn ""
  -- putStrLn $ showProg prg
  putStrLn $ showProg prg'
  void $ interpret prg'

  -- putStrLn ""
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


interpret :: BFProg -> IO (Z.Zipper Word8)
interpret prg
  | mem <- Z.fromList $ replicate memSize (0 :: Word8)
  = fmap snd . flip runStateT mem $ forM_ prg f
  where f = \case
          Move n | n > 0     -> replicateM_ n $ modify Z.right
                 | otherwise -> replicateM_ (-n) $ modify Z.left
          Add  n -> do c <- S.gets Z.cursor
                       S.modify . Z.replace $ fromIntegral n+c
          Out -> S.get >>= liftIO . putChar . toEnum . fromIntegral . Z.cursor
          In  -> liftIO getChar >>= \x -> S.modify $ Z.replace (fromIntegral $ fromEnum x)
          Loop b -> whileM_ ((/=0) <$> S.gets Z.cursor) $ forM_ b f
          Inf -> error "Tried to interpret an infinite loop"

