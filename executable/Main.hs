{-# LANGUAGE MonadComprehensions, LambdaCase #-}

import Binary
import Fucktoid
import Optimization
import Optimizations.BrainFuck as B
import Optimizations.Factor    as F
import Machine

import Control.Monad
import Control.Applicative


main :: IO ()
main = do
  putStrLn ""
  print $ showProg <$> lorem
  putStrLn $ showProg ipsum

abba :: BFProg
abba = parse "+++++ +[>+++++ +++++ +<-]>-.+..-."

test :: Program BrainFuck
test = parse " +++++ > +++++ [-] < -- > +  Simple optimization \
             \ [>]                         Non trivial loop \
             \ +++++ > +++++ [-] < -- > +  More simple stuff \
             \ .                           IO to prevents us from NOPing \
             \                             the entire program \
             \ +++++ +[>+++++ +++++ +<-]>- This should be voided \
             \ "


-- Pop pure ops from to right
popPure :: Machine (Op BrainFuck) ()
popPure
  = greedy' $ do
      left
      guard . isPure =<< cursor
      popr

-- Single pass optimizer
optimize :: Opt BFProg BFProg
optimize = yes
        .> B.optimize
        .> jump (greedy $ oneOf [joinGroups, unsafeClearCell])
        .> inLoop (jump $ greedy joinGroups)

-- Optimization machine
optimizer :: Machine (Op BrainFuck) ()
optimizer = do
  try' $ end >> popPure >> start
  greedy' $ do
    optr' Main.optimize
    right


lorem :: Maybe BFProg
lorem = runOpt Main.optimize test

ipsum :: BFProg
ipsum = runMachine' optimizer test
