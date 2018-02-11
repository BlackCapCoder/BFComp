module Machine2 where

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.List.Zipper as Z
import Control.Applicative
import qualified Control.Monad.Fail as Fail

type Tape = Zipper
newtype Machine s a b = Machine { unMachine :: a -> MaybeT (State (Tape s)) b }


