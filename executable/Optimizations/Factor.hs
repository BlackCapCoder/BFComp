{-# LANGUAGE MonadComprehensions, TypeFamilies #-}
module Optimizations.Factor where

import Fucktoid
import Optimization

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Map as M
-- import GHC.Word (Word8)
import Data.List
import Data.Maybe (catMaybes)


data Factor

instance Fucktoid Factor where
  data Op Factor
    = Group
      { ptr :: Int           -- Current position
      , mem :: M.Map Int Int -- Memory relative to pointer
      }
    | Clear
    | Scan Int

  get = oneOf
    [ [ (Group n mempty, xs) | (Move n:xs) <- id ]
    , [ (Group 0 $ M.singleton 0 n, xs) | (Add n:xs) <- id ]
    , [ (Clear, xs) | (Loop [Add n]:xs) <- id, odd n ]
    , [ (Scan n, xs) | (Loop [Move n]:xs) <- id ]
    ]

  put Clear    = [Loop [Add (-1)]]
  put (Scan n) = [Loop [Move n]]
  put (Group p m)
    | (i, xs) <- foldr (\(i, x) (ix, xs) ->
                         (i, xs ++ (if i==ix then id else (Move (i-ix) :)) [Add x])
                       ) (-p, []) . sortOn (negate . fst) $ M.toList m
    = xs ++ if i==0 then [] else [ Move (-i) ]


joinGroups :: POpt Factor
joinGroups = do
  (Group pa ma:Group pb mb:xs) <- id

  let pc  = pa + pb
      ma' = M.mapKeys (subtract pb) ma
      mb' = {- M.mapKeys id -} mb
      mc  = M.filter (/=0) $ M.unionWith (+) ma' mb'

  return $ Group pc mc : xs


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
-- Assumes the map does not contain zero cells
-- unsafeScan' :: POpt Factor
-- unsafeScan' = [ undefined
--               | (Group p m:Scan n:xs) <- id
--               , r@(_:_) <- pure $ catMaybes [ pogo memSize (p+d) p | d <- M.keys m ]
--               , Just q  <- pure $ M.lookup (minimum r) m ]

