{-# LANGUAGE MonadComprehensions #-}
module Optimizations.BrainFuck where

import Fucktoid
import Optimization

import Prelude hiding ((.), id)
import Control.Monad
import Control.Category


getBalance :: Opt BFProg Int
getBalance = sum . map balance <$> id

circular :: Opt BFProg ()
circular = guard . (==0) =<< getBalance

withLoop :: POpt BrainFuck -> POpt BrainFuck
withLoop o = Opt $ \x -> do
  (Loop b:xs) <- pure x
  (:xs) . Loop <$> runOpt o b

-------------

joinAdds, joinMoves :: POpt BrainFuck
joinAdds  = [ Add  (a+b) : xs | (Add  a:Add  b:xs) <- id ]
joinMoves = [ Move (a+b) : xs | (Move a:Move b:xs) <- id ]

nopAdds, nopMoves :: POpt BrainFuck
nopAdds  = [ xs | (Add  0:xs) <- id ]
nopMoves = [ xs | (Move 0:xs) <- id ]

joinAdds', joinMoves' :: POpt BrainFuck
joinAdds'  = greedy joinAdds  .> nopAdds
joinMoves' = greedy joinMoves .> nopMoves

inpOverwrite :: POpt BrainFuck
inpOverwrite = [ In:xs | (Add _:In:xs) <- id ]

nopLoop :: POpt BrainFuck
nopLoop = [ a : xs | (a@(Loop _):Loop _:xs) <- id ]

clearCell :: POpt BrainFuck
clearCell = [ Loop [Add (-1)] : xs | (Loop [Add n]:xs) <- id, odd n, n /= -1 ]
