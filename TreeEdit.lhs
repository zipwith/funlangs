-------------------------------------------------------------------------------
A simple interactive tree editor              Mark P Jones,  September 19, 1996
================================              =================================

Revised Version: January 20, 2019

> module TreeEdit where
> import Data.Char(toLower)
> import Data.List(intersperse)
> import Pic

Part 1: Forests and trees
-------------------------
We use standard datatypes to represent forests of so-called
general trees:

> type Forest = [Node]              -- A Forest is a list of nodes, each of
> data Node   = Node String Forest  -- which has a value and some children.

Here is a simple example:

> myForest     :: Forest
> myForest      = [Node "1"
>                    [Node "1.1"
>                       [Node "1.1.1" []],
>                     Node "1.2" []],
>                  Node "2" [],
>                  Node "3"
>                     [Node "3.1" [],
>                      Node "3.2" [],
>                      Node "3.3" []]]

Part 2: Navigation
------------------
For the purposes of navigation, we need to have a way of
describing positions within a forest.  For any position, we
need to capture:

 o The nodes to the left of the current position (which we
   will keep in a list with rightmost element first, that is,
   in reverse order).

 o The nodes to the right of the current position, also in a
   list.

 o A sequence of levels up the tree, from the current
   position to the root.  We need to know the position within
   each level, which we represent by a triple (left, x,
   right) where left and right are the siblings on either
   side, and x is the value of the dominating node.

This leads naturally to the following datatype definition:

> data Position = Pos [Node] [Level] [Node]
> type Level    = ([Node], String, [Node])

It is fairly easy to convert between forests and positions
(although the position information is lost, because there
can be many different positions within a given forest):

> rootPosition   :: Forest -> Position
> rootPosition f  = Pos [] [] f

> reconstruct               :: Position -> Forest
> reconstruct (Pos ls us rs) = foldl recon (reverse ls ++ rs) us
>  where recon fs (ls,x,rs) = reverse ls ++ [Node x fs] ++ rs

The following function finds the value (if any) associated
with the node on the immediate right of current position:

> rightValue                         :: Position -> Maybe String
> rightValue (Pos _ _ (Node x _ : _)) = Just x
> rightValue _                        = Nothing

There are four functions for moving around in a forest,
either to the left, to the right, up, or down.  In the last
case, there are two possibilities: down the tree on the
immediate left of the current position, or down the tree on
the immediate right.  For simplicity, we will only consider
the latter.  All of these functions could fail if the
requested move is not possible, so the resulting position is
returned in a Maybe type.

> moveUp, moveDown, moveLeft, moveRight :: Position -> Maybe Position

> moveLeft  (Pos ls us rs)
>                = repos ls (\n ns -> Pos ns us (n:rs))

> moveRight (Pos ls us rs)
>                = repos rs (\n ns -> Pos (n:ls) us ns)

> moveDown  (Pos ls us rs)
>                = repos rs (\(Node x cs) ns -> Pos [] ((ls,x,ns):us) cs)

> moveUp    (Pos ls us rs)
>                = repos us (\(as,x,bs) vs -> Pos as vs (make x : bs))
>                  where make x = Node x (reverse ls ++ rs)

Each of these functions works by inspecting a list and
taking some action if it is non-empty  -- which signals that
a move is possible.  We capture this general pattern in the
following repositioning function:

> repos         :: [b] -> (b -> [b] -> Position) -> Maybe Position
> repos []     f = Nothing
> repos (x:xs) f = Just (f x xs)

We will also want simple methods for inserting and deleting
nodes to the right of the current position (we won't bother
with the obvious duals for insertion or deletion on the
left).

> insertNode    :: String -> Position -> Position
> insertNode x (Pos ls us rs)
>                = Pos ls us (Node x [] : rs)

> deleteNode    :: Position -> Maybe Position
> deleteNode (Pos ls us rs)
>                = repos rs (\_ ns -> Pos ls us ns)

As a mildly amusing little extension, we can define a
reflect operator:

> reflect       :: Position -> Position
> reflect (Pos ls us rs) = Pos rs us ls

This could have been used to define moveLeft in terms of
moveRight (or vice versa).

----8<-------
This code is being released in Week 3.  At this point, we
have not covered some of the more advanced techniques and
features of Haskell that are used in the following code;
we will get to them later, but in the meantime, you are
not expected to read, understand, or modify anything in
the rest of this file!

Part 3: User interface
----------------------
The main interactive process is defined as follows:

> mip  :: Position -> IO ()
> mip p = do putStr "\ESC[2J\ESC[1;1;H"
>            drawForest (reconstruct (insertNode "<*>" p))
>            putStr "Insert Delete Next Back Parent Child Reflect Quit: "
>            line <- getLine
>            putStr "\n"
>            case line of
>              (c:cs) -> command (toLower c) p
>              []     -> do putStrLn "Please choose a command!"
>                           mip p

> command      :: Char -> Position -> IO ()
> command 'c' p = tryTo p moveDown  noNode        -- basic movement
> command 'n' p = tryTo p moveRight noNode 
> command 'b' p = tryTo p moveLeft  noPrev 
> command 'p' p = tryTo p moveUp    noPar  
> command 'd' p = tryTo p deleteNode noNode       -- delete and insert
> command 'i' p = do putStrLn "Type key for new node followed by <RETURN>"
>                    key <- getLine
>                    mip (insertNode key p)
> command 'r' p = mip (reflect p)                 -- a reflection
> command 'q' p = return ()                       -- quit command
> command _   p = do putStrLn "Error: unrecognized command"
>                    mip p

This definition has been simplified by abstracting out a
common pattern for dealing with the results of Maybe types:

> tryTo       :: Position -> (Position -> Maybe Position) -> String -> IO ()
> tryTo p f e  = case f p of
>                  Nothing -> putStrLn e >> mip p
>                  Just x  -> mip x

I have also abstracted out the strings produced by some of
the error messages:

> noNode = "Error: not at node"
> noPrev = "Error: no previous sibling"
> noPar  = "Error: node has no parent"

A simple program that starts up the tree editor at the root
of myForest can be defined as follows:

> main = mip (rootPosition myForest)

We use the Pic library to create some code for drawing trees:

> instance Tree Node where
>   label (Node a f)    = a
>   subtrees (Node a f) = f

> drawForest :: Forest -> IO ()
> drawForest  = putStrLn                -- display picture on screen
>             . show                    -- render picture as a string
>             . foldr top empty         -- join the pictures at the top
>             . intersperse (hstrut 2)  -- add spacing between pictures
>             . map treeToPic           -- a picture for each tree

-------------------------------------------------------------------------------
