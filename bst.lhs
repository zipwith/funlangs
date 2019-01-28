> import Prelude hiding (lookup)

Here is a datatype for simple binary search trees:

> data Tree = Leaf | Fork Tree Int Tree
>   deriving Show

As written, this definition describes binary trees, but doesn't
enforce the requirement that smaller values always appear to the
left of larger values.  Instead, we have to make sure that this
condition is satisfied when we insert a single element in to a
tree:

> insert          :: Int -> Tree -> Tree
> insert n Leaf    = Fork Leaf n Leaf
> insert n (Fork l m r)
>      | n <= m    = Fork (insert n l) m r
>      | otherwise = Fork l m (insert n r)

The benefit of doing this is that we can search quickly to see
if a given element is included in the tree, delivering a result
in O(log n) time if the tree is balanced rather than O(n) time
that would result if we were using a list.

> lookup          :: Int -> Tree -> Bool
> lookup n Leaf    = False
> lookup n (Fork l m r)
>      | n < m     = lookup n l
>      | n > m     = lookup n r
>      | n == m    = True

If you give me suitably a typed function to replace each Fork node, and
also a suitably typed value to replace each Leaf, then I can fold over a
Tree in much the same way that I fold over a list:

> foldTree :: t -> (t -> Int -> t -> t) -> Tree -> t
> foldTree leaf fork Leaf
>   = leaf
> foldTree leaf fork (Fork l n r)
>   = fork (foldTree leaf fork l) n (foldTree leaf fork r)

This function can be used in a variety of ways, including:

> sumTree :: Tree -> Int
> sumTree  = foldTree 0 (\l n r -> l + n + r)

> catTree :: Tree -> [Int]
> catTree  = foldTree [] (\l n r -> l ++ [n] ++ r)

For example, now we can describe a simple sort algorithm that
uses insert to add each element in its input to a binary
search tree, and then uses catTree to extract that same list
of elements in sorted order:

> treeSort :: [Int] -> [Int]
> treeSort  = catTree . foldr insert Leaf

