{-# LANGUAGE TypeFamilies, FlexibleInstances, LambdaCase, MonadComprehensions #-}
module Fucktoid where

import Binary
import Optimization

import Control.Lens
import Control.Category ((>>>))
import Data.Function (fix)
import Debug.Trace


data BrainFuck

memSize :: Int
memSize = 30*1000

-- A fucktoid language is a language that can be converted to BrainFuck
-- .. which is technically all turing complete languages because BrainFuck
-- .. is turing complete. But we support non-turing complete languages as well
-- .. because get doesn't need to translate the whole program,
-- .. just the parts that it can describe!
class Fucktoid l where
  data Op l :: *
  put :: Op l -> BFProg
  get :: Opt BFProg (Op l, BFProg)

instance Fucktoid BrainFuck where
  data Op BrainFuck
    = Add  Int
    | Move Int
    | In
    | Out
    | Loop BFProg
  put = pure
  get = [ (x, xs) | (x:xs) <- yes ]

type Program l = [Op l]
type BFProg = Program BrainFuck
type POpt l = Opt (Program l) (Program l)

--------

parse :: String -> BFProg
parse = fst . parse'
  where parse' []       = (mempty, mempty)
        parse' (']':xs) = (mempty, xs)
        parse' xs | Just (o, rst) <- parseOp xs = parse' rst & _1 %~ (o:)
                  | otherwise = parse' $ tail xs

        parseOp (x:xs)
          = case x of
              '+' -> Just (Add 1,     xs)
              '-' -> Just (Add (-1),  xs)
              '>' -> Just (Move 1,    xs)
              '<' -> Just (Move (-1), xs)
              ',' -> Just (In       , xs)
              '.' -> Just (Out      , xs)
              '[' | (y,ys) <- parse' xs -> Just (Loop y, ys)
              _   -> Nothing
        parseOp _ = Nothing

instance Fucktoid a => Show (Op a) where
  show = (put >>>) . concatMap $ \case
    Add n   | n > 0 -> replicate n '+'
            | n < 0 -> replicate (abs n) '-'
            | otherwise -> "(+-)"
    Move n  | n > 0 -> replicate n '>'
            | n < 0 -> replicate (abs n) '<'
            | otherwise -> "(<>)"
    In     -> ","
    Out    -> "."
    Loop b -> '[' : foldr (\x acc -> show x ++ acc) "" b ++ "]"

showProg :: Fucktoid a => [Op a] -> String
showProg = (>>= show)

-----------

-- Greedy get
gets :: Fucktoid l => Opt BFProg (Program l, BFProg)
gets = do
  (a, b) <- get
  return . flip fix ([a], b) $ \r (l, rst) ->
    case runOpt get rst of
      Just (a',b') -> r (l++[a'], b')
      _         -> (l, rst)

-- Language agnostic gets
gets' :: (Fucktoid a, Fucktoid b) => Opt (Program a) (Program b, Program a)
gets' = [ (b, as) | Just (b, bfs) <- runOpt gets . puts <$> yes
                  , Just (as, []) <- pure $ if null bfs
                                            then pure ([], [])
                                            else runOpt gets bfs ]

-- Translate a program into a BrainFuck program
puts :: Fucktoid a => Program a -> Program BrainFuck
puts = (>>= put)

-- Language agnostic puts
puts' :: (Fucktoid a, Fucktoid b) => Program a -> Maybe (Program b)
puts' a = [ b | (b, []) <- runOpt gets' a ]

-- Run an optimization for another language
jump :: Fucktoid a => Opt (Program a) (Program a) -> Opt (Program BrainFuck) (Program BrainFuck)
jump o = [ x ++ b | (a, b) <- gets, Just x <- pure $ puts <$> runOpt o a ]

-- Language agnostic jump
jump' :: (Fucktoid a, Fucktoid b) => Opt (Program a) (Program a) -> Opt (Program b) (Program b)
jump' o = [ x ++ b | (a, b) <- gets', Just x <- pure $ puts' =<< runOpt o a ]

-- We don't need to translate BrainFuck into BrainFuck
{-# RULES
"jump'/jump" jump' = jump
"gets'/gets" gets' = gets
"puts'/Just" puts' = Just
  #-}

{-# NOINLINE [0] jump' #-}
{-# NOINLINE [0] puts' #-}
{-# NOINLINE [0] gets' #-}


-- The balance is the number of <'s minus the number of >'s
balance :: Op BrainFuck -> Int
balance = \case
  Move n -> n
  Loop b -> sum $ balance <$> b
  _      -> 0

isPure :: Op BrainFuck -> Bool
isPure = \case
  In     -> False
  Out    -> False
  Loop b -> all isPure b
  _      -> True
