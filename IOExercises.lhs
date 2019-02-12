-------------------

> import IOActions

-------------------
1) How many Haskell source files are there in the
current directory?

> problem1 :: IO ()
> problem1
>  = undefined

(It looks like we're going to need this more than once,
so it might be useful to define a reusable IOAction for
getting the list of all Haskell files in the current
directory:)

> haskellFiles :: IO [FilePath]
> haskellFiles
>  = undefined

-------------------
2) How many lines of Haskell source code are in
the current directory?

> problem2 :: IO ()
> problem2
>  = undefined

(Again, it looks like there's another place where
it's useful to abstract out an IOAction that we
can use to compute the length of a file, so I'll
abstract it out so that it can be more easily
reused:)

> fileLength :: FilePath -> IO Int
> fileLength f
>  = undefined

-------------------
3) What is the largest Haskell source file in the
current directory

> problem3 :: IO ()
> problem3
>  = undefined

(Yet again, peeking ahead at the next part, I
realized that this would not be the last time I'd
want to find the largest Haskell file ...)

> largestHaskellFile :: IO FilePath
> largestHaskellFile
>  = undefined

-------------------
4) Copy the largest Haskell source file in the
current directory into Largest.hs

I object, that wasn't a question!  But I know what
you mean ... :-)

> problem4 :: IO ()
> problem4
>  = undefined

-------------------
