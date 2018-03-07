module Math.Unroll where

import BrainFuck (memSize)

{-
  Unrolls a flat, non-balanced loop. E.g:
    + [>+]

  The loop will run 30000 * 255 - 1 times, and every cell except the
  first one will be 255

  The value of a cell after t turns can be written as:
    i + mX * (div fX t) % 256

  where i is the initial value of the cell,
  mX is value X of the loop, and fX is how often
  mX will land on the cell
-}

unroll
  init -- Initial memory
  bal  -- Loop balance
  ms   -- Values added relative to pointer
  = undefined
 where (z,cm) = divMod memSize bal
       o      = mod (bal*z+bal) memSize -- First overshot
       os     = if o==0 then [0] else take bal $ iterate ((`mod`bal).(+o)) 0

