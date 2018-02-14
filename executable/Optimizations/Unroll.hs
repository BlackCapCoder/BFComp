module Optimizations.Unroll where

{-
  Unrolls a flat, non-balanced loop. E.g:
    [++>+>+++>+++<<-]
-}

unroll init ms s
  | l <- length ms
  = undefined
