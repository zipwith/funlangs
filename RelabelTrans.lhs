To load this file in Hugs, use:  hugs -98 RelabelTrans.lhs
No special flags are needed to use this file from GHCi.
----------------------------------------------------------------------

> module RelabelTrans where

This file contains a variant of the code in Relabel.lhs.  It has been
modified to use monad transformers instead of using a hand-crafted
monad.  The benefit of this approach is that we can use an existing
library to help with the task of constructing a suitable monad.  In
this particular case, we'll use the standard library in
Control.Monad.State, which is included as part of the Haskell platform
(as provided by the mtl package in Hackage).

> import Relabel hiding (M, output, labelM)
> import Control.Monad.State

----------------------------------------------------------------------
We'll define our monad M as a state monad with an Int state type, and
built on the basic IO monad:

> type M = StateT Int IO

The primitive operations for accessing the state component of this
monad are put and get.  We can use these operations to implement a
replacement for upd:

> upd  :: (Int -> Int) -> M Int
> upd f = do n <- get; put (f n); return n

Because we have transformed the IO monad using StateT to build the M
monad, we need to apply lift to basic operations in IO to work on M:

> output :: String -> M ()
> output  = lift . putStrLn

Now we can implement the labeling operation on Trees as follows:

> labelM   :: Show a => Tree a -> IO (Tree (Int, a))
> labelM t = do (x, _) <- runStateT (relab t) 0; return x
>  where relab             :: Show a => Tree a -> M (Tree (Int, a))
>        relab Nil          = return Nil
>        relab (Fork x l r) = do output ("Visiting " ++ show x)
>                                n   <- upd (1+)
>                                l'  <- relab l
>                                r'  <- relab r
>                                return (Fork (n,x) l' r')

----------------------------------------------------------------------
