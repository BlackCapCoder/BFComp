{-# LANGUAGE MonadComprehensions #-}
module Optimizations.BrainFuck where

import Binary
import Fucktoid
import Optimization
import TapeMachine

import Prelude hiding ((.), id)
import qualified Data.List.Zipper as Z
import Control.Monad
import Control.Applicative
import Control.Category
import Debug.Trace


getBalance :: Opt BFProg Int
getBalance = sum . map balance <$> id

circular :: Opt BFProg ()
circular = guard . (==0) =<< getBalance

-- inLoop :: POpt BrainFuck -> POpt BrainFuck
-- inLoop o = opt $ \x -> do
--   (Loop b:xs) <- pure x
--   Just r <- pure $ (:xs) . Loop <$> runOpt o b
--   return r

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

clearCell :: POpt BrainFuck
clearCell = [ Loop [Add (-1)] : xs | (Loop [Add n]:xs) <- id, odd n, n /= -1 ]

nopLoop :: POpt BrainFuck
nopLoop = [ a : xs | (a@(Loop _):Loop _:xs) <- id ]

nopAll :: POpt BrainFuck
nopAll = [ pure Inf | (Inf:x:_) <- id ]

-- Without IO a program is just generating heat
-- We can pop pure OPs from the end of the program
-- WARNING: May not be used inside loops
unsafePopPure :: POpt BrainFuck
unsafePopPure = [ [] | xs <- id, not $ null xs, all isPure xs ]

-- WARNING: May only be used at the beginning of the program
unsafePopInit :: POpt BrainFuck
unsafePopInit = oneOf
  [ [ xs | (Loop _:xs) <- id ]
  , [ xs | (Move _:xs) <- id ]
  ]


optimize = do
  -- x <- id
  -- trace (show x) yes
  greedy $ oneOf
    [ joinAdds', joinMoves'
    , inpOverwrite
    , greedy nopLoop
    , nopAll
    -- , inLoop $ do
    --     x <- id
    --     let t = Z.toList . snd . runTape (greedy $ optr' optimize <|> right) () $ Z.fromList x
    --     guard $ x /= t
    --     return $ t
    ]


