{-# LANGUAGE MonadComprehensions, TypeFamilies #-}
module Optimizations.Factor where

import Fucktoid
import Optimization

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Map as M
-- import GHC.Word (Word8)
import Data.List


data Factor

instance Fucktoid Factor where
  data Op Factor
    = Group
      { ptr :: Int           -- Current position
      , mem :: M.Map Int Int -- Memory relative to pointer
      }
    | Clear

  get = oneOf
    [ [ (Group n mempty, xs) | (Move n:xs) <- id ]
    , [ (Group 0 $ M.singleton 0 n, xs) | (Add n:xs) <- id ]
    , [ (Clear, xs) | (Loop [Add n]:xs) <- id, odd n ]
    ]

  put Clear = [Loop [Add (-1)]]
  put (Group p m)
    | (i, xs) <- foldr (\(i, x) (ix, xs) ->
                         (i, xs ++ (if i==ix then id else (Move (i-ix) :)) [Add x])
                       ) (-p, []) . sortOn (negate . fst) $ M.toList m
    = xs ++ if i==0 then [] else [ Move (-i) ]


clearCell :: POpt Factor
clearCell = [ Group p (M.delete 0 m) : xs | (Group p m:Clear:xs) <- id ]

joinGroups :: POpt Factor
joinGroups = do
  (Group pa ma:Group pb mb:xs) <- id

  let pc  = pa + pb
      ma' = M.mapKeys (subtract pb) ma
      mb' = {- M.mapKeys id -} mb
      mc  = M.filter (/=0) $ M.unionWith (+) ma' mb'

  return $ Group pc mc : xs

