------------------------------------------------------------------------

> module Set where

> type Set a    = a -> Bool

> isElem       :: a -> Set a -> Bool
> x `isElem` s  = s x

> univ         :: Set a
> univ          = \x -> True

> empty        :: Set a
> empty         = \x -> False

> singleton    :: Eq a => a -> Set a
> singleton v   = \x -> (x==v)

> infixr 3 /\
> infixr 2 \/

> (\/)         :: Set a -> Set a -> Set a
> s \/ t        = \x -> s x || t x

> (/\)         :: Set a -> Set a -> Set a
> s /\ t        = \x -> s x && t x

------------------------------------------------------------------------
