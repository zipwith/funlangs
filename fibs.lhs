Here is a typical way to implement the Fibonacci function
using straightforward recursion:

> fib  :: Integer -> Integer
> fib n = if n<2 then n else fib (n-1) + fib (n-2)

Now we can use this to produce a long list of Fibonacci
numbers:

    Main> map fib [0..30]
    [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,
    1597,2584,4181,6765,10946,17711,28657,46368,75025,
    121393,196418,317811,514229,832040]
    Main>

If you tried this yourself, you will have found that it
took longer and longer to generate each new number; the
calculation of any single Fibonacci number in this way
is quite expensive.  But we can use the observation that
each Fibonacci number is the sum of its two predecessors
to calculate these numbers more efficiently:

> itFib  :: Integer -> Integer
> itFib n = loop n 0 1
>  where loop m a b = if m==0
>                       then a
>                       else loop (m-1) b (a+b)

The resulting definition is not as clear (i.e., it is
harder to see how this works, or even if it is correct),
but we can make two observations:

1) It produces the same results, at least for small,
nonnegative inputs, as the original version:

    Main> and [ fib n == itFib n | n <- [0..20] ]
    True
    Main>

2) It produces results much faster (be prepared to hit ^C
after you enter this expression --- I wasn't very quick and
the actual output that I got was several pages longer than
what you see here):

    Main> map itFib [0..]
    [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,
    2584,4181,6765,10946,17711,28657,46368,75025,121393,
    196418,317811,514229,832040,1346269,2178309,3524578,
    5702887,9227465,14930352,24157817,39088169,63245986,
    102334155,165580141,267914296,433494437,701408733,
    1134903170,1836311903,2971215073,4807526976,
    7778742049,12586269025,20365011074,32951280099,
    53316291173,^C{Interrupted!}
    Main>

Another way to create this list is to make the following
observations:

- The list begins with 0 and then 1.

- After that, all elements are the sum of the previous
  two elements.

This leads to the following definition:

> fibs :: [Integer]
> fibs  = 0 : 1 : zipWith (+) fibs (tail fibs)

To see how this works, let's write the list of Fibonacci
numbers:

    fibs = [ 0,   1,   1,   2,   3,   5,   8,  13,  21, ....

Now let's write the tail of the same list:

    tail fibs
         = [ 1,   1,   2,   3,   5,   8,  13,  21, ....

And finally, let's add up the corresponding pairs of numbers in
each of the two lists (this is what the call to `zipWith (+)`
does in the definition above):  [See the note about `zipWith`
below if you're not already familiar with that function.]

    zipWith (+) fibs (tail fibs)
         = [ 1,   2,   3,   5,   8,  13,  21, ....

Of course, this is just the list of Fibonaccii numbers, after
the initial 0 and 1 elements!  So now you can see that:

    fibs = 0 : 1 : ... the rest of the Fibonacci numbers ...
         = 0 : 1 : zipWith (+) fibs (tail fibs)

which is how we arrived at the definition above.   Let's just do
a little work to check that this is producing the numbers we
expect:

    Main> fibs == map itFib [0..]
    ^C{Interrupted!}

    Main>

You can see that this expression never terminated; I had to
press ^C to stop it ... the fact that it didn't halt before then
is an indication that it didn't find any cases where the list
elements were different --- if it had, then we would have been
able to conclude immediately that the two lists were not equal,
and we would have seen a `False` result.  But it is still a
little dissatisfying that we can't tell exactly how many test
cases were tried (i.e., how many elements of the two lists were
compared).  The following expression gives us one way to check a
known number of elements (in this case, exactly 1000), and also
provides another illustration of the `zipWith` function:

    Main> and (zipWith (==) fibs (map itFib [0..1000]))
    True
    Main> 

Without trying it in the interpreter, at least initially, can
you see why we used `zipWith` here?  Why, for example, didn't we
just try to evaluate:  fibs == map itFib [0..1000] instead?

-----
A note about `zipWith`

The `zipWith` function is defined in the Haskell standard
prelude, and its definition looks something like this:

    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
    zipWith f _      _      - []

It is a generalization of the `map` function that takes two
input lists and produces one output list by applying the given
function to corresponding pairs of elements in the two inputs.
For example:

    zipWith f [a1, a2, a3] [b1, b2, b3]
     = [f a1 b1, f a2 b2, f a3 b3]

The length of the output list is equal to the length of the
shorter input list: in other words, the zipping process stops as
soon as one of the lists "runs out" of elements.

-----
