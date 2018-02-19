{-# LANGUAGE MonadComprehensions #-}
module Math.Pogo where

import Debug.Trace

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

-- Worst case: O(c)
solvePogo c d l
  | l  < 0 = solvePogo c (c-d) (-l)
  | d  < 0 = solvePogo c (c+d) l
  | l  > c = solvePogo c d $ mod l c

  | y  == 0 = Just (c,d,l,x, getT c d l x)
  | cm == 0 = Nothing
  | ((t,h):_) <- os = Just (c,d,l,h,t)
  | otherwise = Nothing
  where (x,y)  = divMod d l
        (z,cm) = divMod c l

        -- How much i overshot the edge by
        o = mod (l*z+l) c

        -- Exhaustively search all future overshots
        os = [ (t, getH c d l t)
             | (t, x) <- zip [1..] $ takeWhile (/=0) [ mod (o*n) l | n <- [1..] ]
             , {- trace (show (x,t)) $ -} mod (d-x) l == 0 ]

pogo c d l = [ h | (_,_,_,h,_) <- solvePogo c d l ]


-- Only works when l<d
-- Somehow GHC optimizes the first one to be faster than this
solvePogo' c d l
  | l  < 0 = solvePogo' c (c-d) (-l)
  | d  < 0 = solvePogo' c (c+d) l
  | l  > c = solvePogo' c d $ mod l c

  | y  == 0 = Just (c,d,l,x, getT c d l x)
  | cm == 0 = Nothing
  | Just t <- seekDown 1 = Just (c,d,l,getH c d l t,t)
  | ((t,h):_) <- seek2 = Just (c,d,l,h,t)
  | otherwise = Nothing
  where (x,y)  = divMod d l
        (z,cm) = divMod c l

        -- How much i overshot the edge by
        o = mod (l*z+l) c

        -- Only checks the first item in falling sequences of overshoots.
        -- This still has a worst case performance of O(c), but a much better average case
        -- (except that GHC is a beast, and it turns out this runs slower in practice,
        -- despite doing less operations)
        seekDown t
          | x == d = Just t
          | d == x-n*cm = Just $ t + n
          | mod x cm == 0 = Nothing
          | otherwise = trace (show (x,t)) $ seekDown $ t + 1 + div x cm
          where x = mod (o*t) l
                n = div (x-d) cm

        -- I'm not even sure what I am doing at this point.
        -- I think it has the potential to become O(log n)
        seek2
          = [ (t, getH c d l t)
            | t <- [0..cnt-1]
            , x <- pure $ o + mod (t*s) cnt
            , x-cm*div (x-d) cm==d ]
          where cnt = l-o
                s   = mod o (mod l o)

seeksim c d l = [ (t, div t m, t-d-m*div (t-d) m, s)
                | i <- [0..cnt-1]
                , t <- pure $ o + mod (i*s) cnt
                ]
  where (z,m) = divMod c l
        o = mod (l*z+l) c
        cnt = l-o
        s = mod o (mod l o)
-- , mod (s*7) l

{-
  x = o + mod (i*s) cnt

  d = -m*floor((t-d)/m) + t - d
  d = m*ceiling((d - t)/m) + t
-}


checkA d t o l = mod (d - mod (o*t) l) l == 0
checkB d t o l = d - t*o - l * div (d-t*o) l == 0
checkC d t o l = l * div (d-o*t) l + o*t == d

{-
D = h*l - t*c = (h*l - c*floor((h*l)/c)) = l*floor((d - o*t)/l) + o*t
T = (h*l-d)/c = (floor((h*l)/c)) = floor(d/c + t)
H = (d+t*c)/l = (c*floor(d/c + t))/l + d/l
L = (d+t*c)/h = -c*floor(d/c + t) + c*t + d
C = (h*l-d)/t = (h*(h*l - ((h*l - d)*floor((h*l*t)/(h*l - d)))/t))/t - d/t
o = l*floor(c/l) - c*floor((floor(c/l)*l)/c + l/c) + l
0 = d - t*o - l * floor((d-t*o)/l)
d = l*floor((d - o*t)/l) + o*t
-}

overs (x:xs)
  = map head
  . tail
  . snd
  $ foldr (\x (i, l:ls) -> if i>x then (x, (x:l):ls) else (x, [x]:l:ls) ) (x, [[x]]) xs
