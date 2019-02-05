Subtleties of Haskell type inference
------------------------------------

1) Can you write a more general type signature for the
function g in the following example?

> f x = let g  :: Int -> [Int]
>           g y = [x, y]
>       in  g x

2) We can define a function for creating singleton lists:

> box  :: a -> [a]
> box x = [x]

The following expression has type [[Bool]]

> boxTrue  = box (box True)

In theory, (\f -> f (f True)) box = box (box True), which
is the same as boxTrue given above.  By what happens when
we uncomment the following definition?

> --boxTrue1 = (\f -> f (f True)) box

3) In many cases, a Haskell compiler/interpreter can infer
types of defined variables automatically if we leave them
out.  But what happens if we comment out the first line in
the following definition?

> poly    :: [a] -> Bool
> poly xs  = null xs || poly [xs]

(This is an example of "polymorphic recursion": if we call
this function with an argument of type [T] for some type T,
the recursive call has an argument of type [[T]] ...)
