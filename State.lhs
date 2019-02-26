----------------------------------------------------------------------
A simple library for state-based computations
----------------------------------------------------------------------

> module State(ST, run, get, set, upd) where

----------------------------------------------------------------------
Now we define a "state" monad to reflect the way in which stateful
computations transform an initial state into a final state value
in the process of computing a result.  In this code, "s" stands for
the type of the state, while "a" represents the type of the result
that is being produced.

> newtype ST s a = ST (s -> (a, s))

An operation for running a state-based computation (first argument)
with an initial state (second argument):

> run       :: ST s a -> s -> (a, s)
> run (ST t) = t

Definition of the return and bind constructs:

> instance Monad (ST s) where
>   return x = ST (\s -> (x, s))
>   c >>= f = ST (\s -> let (x, s') = run c s in run (f x) s')

Uncomment the following two lines if you are using GHC or another
recent Haskell system:

> {-
> instance Applicative (ST s) where
>   pure      = return
>   cf <*> cx = do f <- cf
>                  x <- cx
>                  return (f x)

> instance Functor (ST s) where
>   fmap f cx = do x <- cx; return (f x)
> -}

It is easy to describe basic state actions that:

- Return the current state:

> get   :: ST s s
> get    = ST (\s -> (s,s))

- Set the current state to a new value:

> set   :: s -> ST s ()
> set s  = ST (\_ -> ((), s))

- Update the current state by applying a function to it:

> upd   :: (s -> s) -> ST s s
> upd f  = ST (\s -> (s, f s))

The functions shown above are generic: in the sense that they would
make sense for any state-based computation, changing the specific
state type, s, that is used in any particular application.  That is
why we have packaged up the code shown here in a separate module.

----------------------------------------------------------------------
