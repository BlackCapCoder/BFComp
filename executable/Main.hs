{-# LANGUAGE MonadComprehensions, LambdaCase #-}
import Fucktoid
import Optimization
import Optimizations.BrainFuck as B
import Optimizations.Factor    as F


main :: IO ()
main = do
  putStrLn ""
  print $ showProg <$> lorem

abba :: BFProg
abba = parse "+++++ +[>+++++ +++++ +<-]>-.+..-."

test :: Program BrainFuck
test = parse " +++++ > +++++ [-] < -- > + . [>] "

lorem :: Maybe BFProg
lorem = runOpt Main.optimize test


-- Single pass optimizer
optimize :: Opt BFProg BFProg
optimize = yes
        .> unsafePopPure
        .> B.optimize
        .> jump (greedy $ oneOf [joinGroups, unsafeClearCell])
        .> inLoop (jump $ greedy joinGroups)

