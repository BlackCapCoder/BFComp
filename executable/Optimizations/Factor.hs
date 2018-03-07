{-# LANGUAGE MonadComprehensions, TypeFamilies, MultiParamTypeClasses #-}
module Optimizations.Factor where

import Language
import BrainFuck

import Math.Pogo

import Control.Category
import Data.List
import Data.Maybe (catMaybes)
-- import GHC.Word (Word8)
import Prelude hiding ((.), id)
import qualified Data.Map as M
import Control.Applicative
import Debug.Trace


data Factor

instance Language Factor where
  data Op Factor
    = Group
      { ptr :: Int           -- Current position
      , mem :: M.Map Int Int -- Memory relative to pointer
      }
    | Clear
    | Scan Int
    | Balanced [Op Factor]
    | FInf
    deriving (Show)

instance Translatable BrainFuck Factor where
  transOp = oneOf
    [ op [ Group n mempty | Move n <- yes ]
    , op [ Group 0 $ M.singleton 0 n | Add n <- yes ]
    , op [ Clear | Loop [Add n] <- yes, odd n ]
    , op [ Scan n | Loop [Move n] <- yes ]
    , op [ Balanced b' | l@(Loop b) <- yes
         , balance l == 0
         , isPure l
         , b' <- exec trans b ]
    , op [ Balanced [] | Loop [] <- yes ]
    ]

instance Translatable Factor BrainFuck where
  transOp = oneOf
    [ op [ Loop [Add (-1)] | Clear <- yes ]
    , op [ Loop [Move n] | (Scan n) <- yes ]
    , op [ Loop x | (Balanced b) <- yes, x <- exec trans b ]
    , op [ Inf | FInf <- yes ]
    , [ (ys ++ if i==0 then [] else [ Move (-i) ], xs)
      | (Group p m:xs) <- yes
      , (i, ys) <- pure $ foldr (\(i, x) (ix, xs) ->
                                   (i, xs ++ (if i==ix then id else (Move (i-ix) :)) [Add x])
                                ) (-p, []) . sortOn (negate . fst) $ M.toList m
      ]
    ]

transOpF :: Opt BFProg (Program Factor, BFProg)
transOpF = transOp

optimize = greedy $ oneOf
  [ joinGroups
  ]

unsafeOptimize = greedy $ oneOf
    [ joinGroups
    , unsafeClearCell
    , unsafeUnrollBalanced
    , unsafeNopBalanced
    , unsafeNopScan <|> unsafeScan
    ]

joinGroups :: POpt Factor
joinGroups = do
  (Group pa ma:Group pb mb:xs) <- id

  let pc  = pa + pb
      ma' = M.mapKeys (subtract pb) ma
      mb' = {- M.mapKeys id -} mb
      mc  = M.filter (/=0) $ M.unionWith (+) ma' mb'

  return $ Group pc mc : xs

-- WARNING: This may only be used when the memory is known
unsafeNopBalanced :: POpt Factor
unsafeNopBalanced
  =  [ x:xs | (x@(Group _ m):Balanced _:xs) <- id
     , Nothing <- pure $ M.lookup 0 m ]
 <|> [ [FInf] | (x@(Group _ m):Balanced []:_) <- id ]

-- WARNING: This may only be used when the memory is known
unsafeUnrollBalanced :: POpt Factor
unsafeUnrollBalanced = do
  (Group pa ma:Balanced b:xs) <- id
  Just [Group pb mb] <- pure $ runOpt (try unsafeOptimize) b
  Just a <- pure $ M.lookup 0 ma
  let b' = M.lookup 0 mb

  if (pa == pb && M.null mb) || b' == Nothing
     then return [FInf]
     else do
        let Just b = b'

        return $ case pogo 256 a $ negate b of
          Just h
            | mc <- M.unionWith (+) ma $ M.map (normalize.(*h)) mb
            -> Group pb mc:xs
          _ -> [FInf]

normalize :: Int -> Int
normalize x
  | y > 128   = y - 256
  | otherwise = y
  where y = mod x 256



-- WARNING: This may only be used when the memory is known
unsafeClearCell :: POpt Factor
unsafeClearCell = [ Group p (M.delete 0 m) : xs | (Group p m:Clear:xs) <- id ]

-- WARNING: May only be used when the memory is known
-- WARNING: May enter an infinite loop
unsafeScan :: POpt Factor
unsafeScan = [ Group p' (M.mapKeys (+(p'-p)) m) : xs | (Group p m:Scan n:xs) <- id
             , (p':_) <- pure [ x | x <- iterate ((`mod`memSize).(+n)) 0
                                  , case M.lookup x m of
                                      Nothing -> True
                                      _ -> False ] ]

-- WARNING: May only be used when the memory is known
unsafeNopScan :: POpt Factor
unsafeNopScan
  = [ Group p m : xs
    | (Group p m:Scan n:xs) <- id
    , Nothing <- pure $ M.lookup 0 m ]
 <|>
    [ Group p m : Group n mempty : xs
    | (Group p m:Scan n:xs) <- id
    , null $ catMaybes [ pogo memSize (p+d) p
                       | d <- M.keys $ M.delete 0 m ] ]

