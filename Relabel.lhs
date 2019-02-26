----------------------------------------------------------------------

> module Relabel where

> import qualified Treedot as T
> import State

----------------------------------------------------------------------
Here is a basic tree datatype definition:

> data Tree a = Nil | Fork a (Tree a) (Tree a)
>              deriving (Show, Eq)

Here is a non-trivial example that I used on the slides:

> example = Fork 'f' (Fork 'c' (Fork 'a' Nil (Fork 'b' Nil Nil))
>                              (Fork 'e' (Fork 'd' Nil Nil) Nil))
>                    (Fork 'j' (Fork 'h' (Fork 'g' Nil Nil) (Fork 'i' Nil Nil))
>                              (Fork 'k' Nil (Fork 'l' Nil Nil)))

----------------------------------------------------------------------
Here are instances that will allow us to generate descriptions
of Tree values in dot format:

> instance T.Tree (Tree a) where
>   subtrees Nil = []
>   subtrees (Fork n l r) = [l, r]

> instance Show a => T.LabeledTree (Tree a) where
>   label Nil = ""
>   label (Fork n l r) = show n

Now, for example, we can use the following function to write out a
dot description of a Tree to a file on disk:

> wrt  :: Show a => Tree a -> IO ()
> wrt t = writeFile "tree.dot" (T.toDot t)

----------------------------------------------------------------------
Here is a purely functional version of the relabeling code:

> label :: Tree a -> Tree (Int, a)
> label = snd . relab 0
>  where relab               :: Int -> Tree a -> (Int, Tree (Int,a))
>        relab n Nil          = (n, Nil)
>        relab n (Fork x l r) = let (n',  l') = relab (n+1) l
>                                   (n'', r') = relab n'    r
>                               in (n'', Fork (n, x) l' r')

----------------------------------------------------------------------
Here is a version of the same function using the State monad:

> mlabel  :: Tree a -> Tree (Int, a)
> mlabel t = fst (run (relab t) 0)
>  where relab             :: Tree a -> ST Int (Tree (Int, a))
>        relab Nil          = return Nil
>        relab (Fork x l r) = do n <- upd (1+)
>                                l' <- relab l
>                                r' <- relab r
>                                return (Fork (n,x) l' r')

It's not hard to see that the fundamental logic in these two pieces
of code is the same.  The latter, however, has less explicit plumbing
of state values, and hence less opportunity for making errors!

For testing, try:

> main = let ex1 = label example
>            ex2 = mlabel example
>        in do putStr "Results are the same: "
>              print (ex1 == ex2)
>              wrt ex1

----------------------------------------------------------------------
If we want to combine state passing with IO, we could build a custom
monad for the purpose:

> newtype M a = M { unM :: Int -> IO (a, Int) }

> instance Monad M where
>   return x = M (\n -> return (x, n))
>   c >>= f  = M (\n -> do (x, n') <- unM c n 
>                          unM (f x) n')

Uncomment the following two lines if you are using GHC or another
recent Haskell system:

> {-
> instance Applicative M where
>   pure      = return
>   cf <*> cx = do f <- cf; x <- cx; return (f x)

> instance Functor M where
>   fmap f cx = do x <- cx; return (f x)
> -}

> updM      :: (Int -> Int) -> M Int
> updM f     = M (\n -> return (n, f n))

> output    :: String -> M ()
> output s   = M (\n -> do putStrLn s
>                          return ((),n))

> runM      :: M a -> Int -> IO a
> runM c n   = do (x,n') <- unM c n; return x

And now we can implement a version of mlabel that also does IO:

> labelM   :: Show a => Tree a -> IO (Tree (Int, a))
> labelM t = runM (relab t) 0
>  where relab             :: Show a => Tree a -> M (Tree (Int, a))
>        relab Nil          = return Nil
>        relab (Fork x l r) = do output ("Visiting " ++ show x)
>                                n   <- updM (1+)
>                                l'  <- relab l
>                                r'  <- relab r
>                                return (Fork (n,x) l' r')

While it might initially be a pain to write code in the monadic style,
this example shows that subsequent changes can be made without major
overhaul, just by inserting extra operations where they are needed
(like the call to output in this example), and modifying the
definition of the underlying monad to support the new features.

----------------------------------------------------------------------
Look at the code in RelabelTrans if you'd like to see how we can build
a monad that combines State and IO by using a standard library of
"Monad Transformers" instead of the hand-crafted monad M that we have
constructed here.

----------------------------------------------------------------------
