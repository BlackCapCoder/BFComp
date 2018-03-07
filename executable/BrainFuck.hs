{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, LambdaCase, MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BrainFuck where

import Language
import Optimization

import Control.Category ((>>>))
import Control.Lens
import Data.Function (fix)


data BrainFuck
type BFProg = Program BrainFuck

instance Language BrainFuck where
  data Op BrainFuck
    = Add  Int
    | Move Int
    | In
    | Out
    | Loop BFProg
    | Inf
    deriving (Eq)


memSize :: Int
memSize = 30*1000

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

-------

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

instance Show (Op BrainFuck) where
  show = \case
      -- Add n   | n > 0 -> replicate n '+'
      --         | n < 0 -> replicate (abs n) '-'
      --         | otherwise -> "(+-)"
      Add n | n == 1 -> "+"
            | n == (-1) -> "-"
            | n > 0 -> '+' : show n
            | otherwise -> show n
      -- Move n  | n > 0 -> replicate n '>'
      --         | n < 0 -> replicate (abs n) '<'
      --         | otherwise -> "(<>)"
      Move n  | n > 1 -> '(' : show n ++ ">)"
              | n < (-1) -> '(' : show (abs n) ++ "<)"
              | n > 0 -> replicate n '>'
              | n < 0 -> replicate (abs n) '<'
              | otherwise -> "(<>)"

      In     -> ","
      Out    -> "."
      Loop b -> '{' : show (balance $ Loop b) ++ "}" ++ '[' : foldr (\x acc -> show x ++ acc) "" b ++ "]"
      Inf    -> "(Infinite loop)"

showProg :: Translatable a BrainFuck => Program a -> String
showProg p | Just xs <- runOpt trans p
           = show =<< (xs :: Program BrainFuck)

