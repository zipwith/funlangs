The `group` method takes a list of values and splits it into a
list of lists, with each part being some fixed length n.  (If
the length of the original list is not a multiple of n, then the
last list in the output of group will have less than n
elements.)

> group   :: Int -> [a] -> [[a]]
> group n  = takeWhile (not . null)
>          . map (take n)
>          . iterate (drop n)

The following examples show that group produces the expected
results on two simple test cases with groups of size 3 and 4,
respectively:

    Main> group 3 [1..12]
    [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
    Main> group 4 [1..10]
    [[1,2,3,4],[5,6,7,8],[9,10]]
    Main>

These specific two examples have been chosen to illustrate that
the function behaves correctly, both when the the input list's
length is a multiple of the group size, and when it is not.  It
is often useful to test behavior on "edge cases", and in this
situation, we might be concerned about the possibility that the
group size is zero or negative, or that the input list is empty.
For example, with a group size of zero, a buggy implementation
might generate an infinite list of empty lists as its result,
which is probably not what we want.  Happily, it is quite
straightforward to check for examples like this at the
interpreter prompt:

    Main> group 0 [1..10]
    []
    Main> group (-2) [1..10]
    []
    Main> group 3 []
    []
    Main>

We were not provided with a "formal specification" for the
behavior of `group` in these edge cases, but the results shown
here do not seem unreasonable.  (Even if just returning an empty
list might be seen as a little dull :-)

The description of `group` at the top of this file did specify
that each of the sublists should have the same size, with the
possible exception of the last item, which may be smaller.  Of
course, we can count the number of elements in each sublist by
hand, but we can also automate that process by applying `map
length` to each of the examples:

    Main> map length $ group 3 [1..12]
    [3,3,3,3]
    Main> map length $ group 4 [1..10]
    [4,4,2]
    Main>

These outputs show that, at least in these examples, each of the
sublists does have the specified group length, except in the
last element where the length may be smaller.

As a final property, we expect `group` to arrange all of the
elements in its input list in to smaller sublists, but we still
expect the resulting list to contain all of the same elements,
in the same order, as the original input list.  Fortunately we
can attempt to check for this by concatenating the sublists in
the output and then comparing the result with the input:

    Main> concat (group 4 "hello, world, isn't this fun")
    "hello, world, isn't this fun"
    Main> concat (group 17 [1..1000]) == [1..1000]
    True
    Main>

The first of the two examples here shows `group` applied to a
String (which, of course, is just the same as a list of
characters).  By visual inspection, we can see that the output
string is the same as the input.  In the second example, we take
advantage of the `==` operator to perform this check automatically.
Although these examples alone do not guarantee that `group` is
working correctly, the use of `[1..1000]` is a good test because
it does not contain any duplicated elements: if there was an error
in `group` that caused elements of the input to be duplicated,
dropped, or reordered, then those problems would likely become
apparent when running this example.

If we imagine wanting to run large numbers of tests of this kind,
then we might actually define some new functions to capture the
general pattern shown here:

> checkGroup n xs = (concat (group n xs) == xs)

> checkMany xs = and [ checkGroup n xs | n <- ns ]
>   where ns = [1..2+length xs]

The `checkGroup` function just corresponds to a generalization
of the last test above to use parameters `n` and `xs` instead
of specific values `17` and `[1..1000]`.  But `checkMany` takes
this pattern and constructs a range of test cases for a single
string, using every possible group size between `1` and
`2+length xs`.  (There is nothing particular special about the
choice of `2` here; I just wanted a range of test cases that
extended on either side of the set of `sensible` values for `n`
with a given choice of `xs`.)  Now we can run a large number of
test cases automatically with just a few keystrokes:

    Main> checkMany "hello, world"
    True
    Main> and [ checkMany [1..n] | n <- [1..100] ]
    True
    Main> 

The last example here actually runs more than 5,000 tests, and
the final `True` result shows that the code passed every single
one!

When I first wrote the definition of `checkMany` above, I also
included zero in the associated list of `ns` ... and then
quickly discovered that this was not a good idea when my tests
using `checkMany` produced a `False` result.  With hindsight,
this should have been obvious to me from the previous examples,
because we know that `group 0 xs` returns the empty list, and
so we know that test like the following will fail (or, as it
actually appears here, return `False`):

    Main> checkGroup 0 "hello, world"
    False
    Main> 

We end this file with an example from the lecture: defining a
`commas` function that inserts a comma after every third digit
in a string representing a number:

> commas :: Integer -> String
> commas  = reverse
>         . foldr1 (\xs ys -> xs ++ "," ++ ys)
>         . group 3
>         . reverse
>         . show

Again, it is easy to test this at the interpreter prompt, taking
care to include a range of examples, some of which are short
enough not to require any commas, while others require multiple
commas:

    Main> commas 0
    "0"
    Main> commas 100
    "100"
    Main> commas 1000
    "1,000"
    Main> commas 100000000
    "100,000,000"
    Main> commas 1234567890
    "1,234,567,890"
    Main> commas (product [1..24])
    "620,448,401,733,239,439,360,000"
    Main>

Of all these examples, the one for 1234567890 is perhaps the
most interesting/useful because it uses all of the decimal
digits in order, so it would be easiest to see if one or more
digits had been omitted or duplicated.  Beyond that, a quick
visual inspection of the output shows that our `commas` function
is inserting commas after every third digit, just as we wanted!

