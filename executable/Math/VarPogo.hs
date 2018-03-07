module Math.VarPogo
  ( varPogo
  ) where

{-

  This is identical to the pogo problem, except that the
  pogo stick has variable jump lengths.

C  = number of spots on the circle
D  = distance from starting point to the candy
Ls = circular list of jump lengths
H  = number of hops (answer)
T  = number of times passed all the way around the circle

-}

-- Simulate var pogo
simVarPogo c d = takeWhile (/=d) . map (`mod`c) . scanl (+) 0 . cycle

-- Number of iterations to get back to the beginning.
-- If H exist it must be smaller than this number
varPogoMax c ls = c * length ls

-- Number of turns to get back to the beginning
varPogoMaxT c ls = max (s - 1) 0
  where s = sum $ map (`mod`c) ls

-- Brute number of turns, used to vertify function above
simVarPogo' c d ls
  | m <- varPogoMax c ls
  , g <- take m $ map (`mod`c) . scanl (+) 0 $ cycle ls
  = length $ filter id $ zipWith (>) g (tail g)

tst c ls = (simVarPogo' c 0 ls, varPogoMaxT c ls)


-- Solve by bruteforce
varPogoBrute c d ls
  | m  <- varPogoMax c ls
  , gs <- take m $ simVarPogo c d ls
  = if length gs == m then Nothing else Just $ length gs

-- Get distance traveled after n hops
travelLength n ls
  | (d,m) <- divMod n $ length ls
  = sum ls * d + sum (take m ls)

{-

F = travelLength
S = sum ls
L = length ls

D = F(H) - T*C
F(H) = D + T*C
F(H) - D = T*C

F(H) = S*X + Y

X = div H L ~= F(H) (mod H)
Y = F(H) (mod S) = F(H) (mod x)

H ~= X*L + div Y L ~= (F(H)*L - (div Y L)*L) / S
F(H) ~= div (h*s) l + (mod (h*s) l)

-}

varPogo c d ls
  | (x:_) <- ns = Just x
  | otherwise   = Nothing
 where l = length ls
       s = sum ls
       ns = [ (t, f, h)
            | t <- [0..varPogoMaxT c ls]
            , f <- [d + t*c]
            , x <- [div f s]
            , y <- [f - s*x]
            , (ns, gs) <- [span (<y) $ scanl (+) 0 ls]
            , head gs == y
            , h <- [x*l + length ns]
            ]
