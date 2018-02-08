{-# LANGUAGE MonadComprehensions, LambdaCase #-}
import Fucktoid
import Optimization
import Optimizations.BrainFuck as B
import Optimizations.Factor    as F


main :: IO ()
main = do
  putStrLn ""
  print lorem

abba :: BFProg
abba = parse "+++++ +[>+++++ +++++ +<-]>-.+..-."

test :: Program BrainFuck
test = parse " +++++ > +++++ [-] < -- > + "


optimize :: Opt BFProg BFProg
optimize = oneOf [ joinAdds', joinMoves' ]
        .> jump' (greedy $ oneOf [joinGroups, F.clearCell])


lorem :: Maybe BFProg
lorem = runOpt (jump' joinAdds') test
