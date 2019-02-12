--------------------------------------------------------------------------
This file packages up some useful IO Actions from the prelude and some
standard libraries.  This file is intended to be used in exercises for
learning how to use the IO type in Haskell.  If you wanted to use these
functions while you're developing a specific application, then you
might prefer to import most of the operations that you need directly
from the modules where they are actually defined.

This file should load in Hugs or GHC without any special exceptions or
options.

--------------------------------------------------------------------------
Some examples:

How many lines are there in the file "tree.dot"?

  readFile "tree.dot" >>= inIO (length . lines) >>= print

Display a list of strings for the items in the current directory:

  getCurrentDirectory >>= getDirectoryContents >>= print

Display a sorted list of the files in the current directory:

  getCurrentDirectory
    >>= getFiles
    >>= inIO sort
    >>= print

Display the length of the longest literate Haskell source file
in the current directory:

  getCurrentDirectory
     >>= getFiles
     >>= inIO (filter (isSuffixOf ".lhs"))
     >>= mapM (\f -> readFile f >>= inIO (length . lines))
     >>= inIO maximum
     >>= print

Open one of the pdf files in this directory (assumes that the host
operating system has an appropriate "open" command; this works on
Mac OS X, but you will likely need to use a different command on
other OSes).

  getCurrentDirectory
    >>= getFiles
    >>= inIO (filter (isSuffixOf ".pdf"))
    >>= inIO head
    >>= runCommand "open"

Generate a tree diagram showing the structure of the home directory:

  getHomeDirectory >>= getFileSystem 1 >>= graphviz "tree.dot"

--------------------------------------------------------------------------

> module IOActions(

Basic combinators for constructing IO Actions:

>     return,                    -- :: a -> IO a
>     (>>=),                     -- :: IO a -> (a -> IO b) -> IO b
>     inIO,                      -- :: (a -> b) -> a -> IO b
>     mapM,                      -- :: (a -> IO b) -> [a] -> IO [b]
>     mapM_,                     -- :: (a -> IO b) -> [a] -> IO ()
>     filterM,                   -- :: (a -> IO Bool) -> [a] -> IO [a]
>     foldM,                     -- :: (a -> b -> IO a) -> a -> [b] -> IO a
>     foldM_,                    -- :: (a -> b -> IO a) -> a -> [b] -> IO ()
>     replicateM,                -- :: Int -> IO a -> IO [a]
>     replicateM_,               -- :: Int -> IO a -> IO ()

IO Actions for simple terminal and file I/O:

>     putChar,                   -- :: Char   -> IO ()
>     putStr,                    -- :: String -> IO ()
>     putStrLn,                  -- :: String -> IO ()
>     print,                     -- :: Show a => a -> IO ()
>     getChar,                   -- :: IO Char
>     getLine,                   -- :: IO String
>     getContents,               -- :: IO String
>     readFile,                  -- :: String -> IO String
>     writeFile,                 -- :: String -> IO ()

IO Actions for querying the filesystem:

>     FilePath,
>     (</>),                     -- :: FilePath -> FilePath -> FilePath
>     getDirectoryContents,      -- :: FilePath -> IO [FilePath]
>     getDirectoryPaths,         -- :: FilePath -> IO [FilePath]
>     getCurrentDirectory,       -- :: IO FilePath
>     getHomeDirectory,          -- :: IO FilePath
>     doesFileExist,             -- :: FilePath -> IO Bool
>     doesDirectoryExist,        -- :: FilePath -> IO Bool
>     createDirectory,           -- :: FilePath -> IO ()

>     getFiles,                  -- :: FilePath -> IO [FilePath]
>     getDirectories,            -- :: FilePath -> IO [FilePath]

>     FileSystem(..),            -- :: data FileSystem = ...
>     getFileSystem,             -- :: Int -> FilePath -> IO FileSystem

An IO Action for generating a dot output file:

>     graphviz,                  -- :: LabeledTree t => String -> t -> IO ()

IO Actions for interacting with the application's OS environment:

>     system,                    -- :: String -> IO ExitCode
>     runCommand,                -- :: String -> FilePath -> IO ExitCode
>     getArgs,                   -- :: IO [String]
>     getProgName,               -- :: IO String
>     getEnv,                    -- :: String -> IO String

For convenience, we re-export the operations defined in Data.List:

>     module Data.List,          -- :: isPrefixOf, isSuffixOf, ...

>  )  where

Some standard imports:

> import Control.Monad
> import System.IO
> import System.Cmd -- Comment this line out for GHCi
> -- import System.Process hiding (runCommand)  -- Comment this out for Hugs
> import System.Directory
> import System.Environment
> import System.Exit
> import Data.List

Our custom library for generating dot-format descriptions of trees:

> import Treedot

The inIO function turns a pure function into an IO Action (that, in fact,
has no IO effect):

> inIO  :: (a -> b) -> a -> IO b
> inIO f = return . f

The built-in "system" function gives us a way to execute a given command
line string; we define a related "runCommand" function that constructs a
command line by combining the name of a specific program with the path to
a file that is passed in as an argument:

> runCommand :: String -> FilePath -> IO ExitCode
> runCommand cmd path = system (cmd ++ " " ++ show path)

We use the following functions to list the files and subdirectories
in a given folder, filtering out all items whose name begins with a
dot (which includes the current directory, ., and the parent, ..):

> getFiles        :: FilePath -> IO [FilePath]
> getFiles path    = getDirectoryContents path
>                    >>= inIO (filter (not . dotFile))
>                    >>= filterM doesFileExist

> getDirectories     :: FilePath -> IO [FilePath]
> getDirectories path = getDirectoryContents path
>                       >>= inIO (filter (not . dotFile))
>                       >>= filterM doesDirectoryExist

> dotFile           :: FilePath -> Bool
> dotFile ('.':name) = True
> dotFile other      = False

getDirectoryPaths is like getDirectories except that it attempts to provide
full paths for each item instead of a short name relative to the input path:

> getDirectoryPaths :: FilePath -> IO [FilePath]
> getDirectoryPaths path
>                    = getDirectoryContents path >>= return . map (path </>)

We'll use the following utility function for combining file path fragments;
this is not the most robust definition possible, but it will serve well enough
for our purposes in this module:

> (</>)  :: FilePath -> FilePath -> FilePath
> p </> q = p ++ "/" ++ q

We define a datatype for representing the contents of a file system.
For practical purposes, we assume that the resulting tree will be
pruned at some particular depth, and capture the points in the
generated tree that have been pruned by using the Foldep constructor:

> data FileSystem = File FilePath                   -- a single file
>                 | Folder FilePath [FileSystem]    -- a folder/directory
>                 | Foldep FilePath                 -- a pruned entry
>                   deriving Show

Define the basic tree structure of the FileSystem type so that we can
generate dot diagrams for it:

> instance Tree FileSystem where
>   subtrees (File path)      = []
>   subtrees (Folder name cs) = cs
>   subtrees (Foldep path)    = []

> instance LabeledTree FileSystem where
>   label (File path)      = path
>   label (Folder path cs) = path ++ "/"
>   label (Foldep path)    = path ++ "..."

Next comes the main algorithm for traversing an actual file system
and producing the corresponding FileSystem value:

> getFileSystem :: Int -> FilePath -> IO FileSystem
> getFileSystem n path
>  = getFileSystemDir n path path

> getFileSystemDir :: Int -> FilePath -> FilePath -> IO FileSystem
> getFileSystemDir n path name
>  | n < 1     = return (Foldep name)
>  | otherwise = getDirectoryContents path
>                >>= inIO (filter (not . dotFile))
>                >>= mapM (getFileSystemIn (n-1) path)
>                >>= inIO (Folder name)

> getFileSystemIn :: Int -> FilePath -> FilePath -> IO FileSystem
> getFileSystemIn n parent child
>  = doesDirectoryExist path
>    >>= \b-> case b of
>         True  -> getFileSystemDir n path child
>         False -> return (File child)
>    where path = parent </> child

Write a dot-format description of a particular tree to a given file:

> graphviz       :: LabeledTree t => String -> t -> IO ()
> graphviz path t = inIO toDot t
>                   >>= writeFile path
>                   >> runCommand "open -a GraphViz" path
>                   >> return ()

--------------------------------------------------------------------------
