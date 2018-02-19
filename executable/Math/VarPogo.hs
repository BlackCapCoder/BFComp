module Math.VarPogo where

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

ls = [5,4,6,2,81,742,9999,3,78,1]
c  = 30000
d  = 1234
s  = sum ls                            -- 10921
l  = length ls                         -- 10
x  = div f s                           -- 4225
y  = f-s*x                             -- 9
f  = travelLength h ls                 -- 46141234
h  | Just x <- varPogoBrute c d ls = x -- 42252
t  = div (f - d) c                     -- 1538

varPogo c d ls
  | (x:_) <- ns = ns -- x
 where l = length ls
       ns = [ (t, f, h, a+b)
            | t <- [0..div c l]
            , f <- [d + t*c]
            , x <- [div f s]
            , y <- [f - s*x]
            , n <- [length . takeWhile (<y) $ scanl (+) 0 ls]
            , h <- [x*l + n]
            , (a,b) <- [divMod (h*s) l]
            -- , a+b == f -- I am not positive this is correct
            ]
