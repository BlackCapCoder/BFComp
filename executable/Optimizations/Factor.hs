{-# LANGUAGE MonadComprehensions, TypeFamilies #-}
module Optimizations.Factor where

import Fucktoid
import Optimization

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Map as M
-- import GHC.Word (Word8)
import Data.List
import Data.Maybe


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
             , (p':_) <- pure [ x | x <- iterate ((`mod`memSize).(+n)) p
                                  , case M.lookup x m of
                                      Nothing -> True
                                      Just 0 -> True
                                      _ -> False ] ]

-- WARNING: May only be used when the memory is known
-- Assumes the map does not contain zero cells
-- TODO: This can prove non-totality
unsafeScan' :: POpt Factor
unsafeScan' = [ Group q (M.mapKeys (+(q-p)) m) : xs
              | (Group p m:Scan n:xs) <- id
              , r@(_:_) <- pure $ catMaybes [ pogo memSize (p+d) p | d <- M.keys m ]
              , Just q  <- pure $ M.lookup (minimum r) m
              ]


{- This is a simplified version of the scan problem.

  I am standing on a pogo stick at the surface of a circle with an integer circumference C.
  An integer distance away from me D is a piece of candy.
  My pogo stick can only jump in one direction, and only some specific integer length L.
  If I were to pogo forever, would I ever land on the candy, and if so, how many jumps would it take?

Examples:
  (C=10, D=1, L=2) -> impossible
  (C=23, D=3, L=5) -> 19 jumps

C = number of spots on the circle
L = distance of one hop
D = distance from starting point to the candy
H = number of hops (answer)
T = number of times passed all the way around the circle

H*L  how from spots the starting point on the Hth hop
T*C  how many spots take you back to the starting point
        on the Tth time around the circle

D = H*L - T*C       you land on the candy after H hops takes you T times around the circle
D = H*L (modulo C)  another way to express the same, without caring about specific T

-}

simPogo c d l = takeWhile (/=d) $ iterate ((`mod`c).(+l)) 0

solvePogoBrute c d l
  | h <- length $ simPogo c d l
  = (c, d, l, h, getT c d l h)

getT c d l h = div (h*l-d) c
getH c d l t = div (d+t*c) l

solvePogo c d l
  | l  > c = solvePogo c d $ mod l c
  | y  == 0 = Just (c,d,l,x, getT c d l x)
  | cm == 0 = Nothing
  | ((t,h):_) <- os = Just (c,d,l,h,t)
  | otherwise = Nothing
  where (x,y)  = divMod d l
        (z,cm) = divMod c l

        -- How much i overshot the edge by
        o = mod (l*z+l) c

        -- Exhaustively search future overshots
        os = [ (t, getH c d l t)
             | (t, o) <- zip [1..] $ takeWhile (/=0) [ mod (o*n) l | n <- [1..] ]
             , mod (d-o) l == 0 ]

pogo c d l
  | (d', l') <- if l >0 then (d,l) else (c-d, -l)
  , d''      <- if d'>0 then d'    else c+d'
  = [ h | (_,_,_,h,_) <- solvePogo c d'' l' ]
