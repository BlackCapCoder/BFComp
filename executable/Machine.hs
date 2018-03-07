module Machine where

import Binary

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Fail as Fail


type Machine s = Kleisli (MaybeT (State s))


runMachine :: Machine s a b -> a -> s -> (Maybe b, s)
runMachine k = runState . runMaybeT . runKleisli k

runMachine' :: Machine s s b -> s -> (Maybe b, s)
runMachine' m s = runState (runMaybeT $ runKleisli m s) s

machine = Kleisli


instance Functor (Machine s a) where
  fmap f m = machine $ fmap f . runKleisli m

instance Applicative (Machine a b) where
  pure = machine . const . pure
  f <*> o = machine $ liftA2 ap (runKleisli f) (runKleisli o)

instance Monad (Machine a b) where
  o >>= f = machine $ \x -> runKleisli o x >>= flip runKleisli x . f
  fail = Fail.fail

instance Fail.MonadFail (Machine a b) where
  fail _ = empty

instance Alternative (Machine a b) where
  empty = machine $ const empty
  a <|> b = machine $ \x -> runKleisli a x <|> runKleisli b x

instance Monoid (Machine a b c) where
  mempty = empty
  mappend = (<|>)

instance MonadState a (Machine a b) where
  get   = machine $ const get
  put   = machine . const . put
  state = machine . const . state

instance Binary (Machine a) b where
