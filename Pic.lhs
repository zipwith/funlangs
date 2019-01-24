> module Pic where

The code in this file provides a library for drawing text
pictures, suitable for display on a terminal screen with a
fixed width font.

-----------------------------------------------------------
We start by importing some general list processing operators
from one of the standard Haskell libraries:

> import Data.List(transpose,   -- transpose a list of lists
>                  intersperse) -- add an element between each 
>                               -- adjacent pair of values in a list

-----------------------------------------------------------
A (text) picture is described by specifying its height
(number of rows), width (number of columns), and the
specific characters within that block (represented by a
list of strings, one for each row of the picture):

> data Pic = Pic Int Int [String]   -- height, width, image data
>  deriving (Eq)

We provide functions for accessing the height and the width
of an arbitrary picture:

> height              :: Pic -> Int
> height (Pic h w img) = h

> width               :: Pic -> Int
> width (Pic h w img)  = w

For example, we can construct pictures that contain a single
line/string:

> string              :: String -> Pic
> string s             = Pic 1 (length s) [s]

We can also construct "struts", which are pictures in which
one of the dimensions is zero; you might wonder why we would
include these because they don't have any visible
appearance, but they can sometimes be useful when we want to
control the layout of multiple subpictures within a larger
picture.  A horizontal strut has width, but no height:

> hstrut   :: Int -> Pic
> hstrut w  = Pic 0 w []

A vertical struct has height, but no width (and hence has an
empty row of text for each of the specified height lines):

> vstrut   :: Int -> Pic
> vstrut h  = Pic h 0 (replicate h [])

As a special case (of both hstrut and vstrut), we can
construct an empty picture that has no height or width:

> empty :: Pic
> empty  = hstrut 0

We can also define a function for displaying pictures on the
screen:

> instance Show Pic where
>   show (Pic h w img) = unlines img

Next, we would like to define some operators for arranging a
pair of pictures in a vertical stack.  If the two pictures
that we are combining have the same width, then this is a
straightforward operation: we can just concatenate the image
data from the top picture with the image data from the
second.  But what if one image has a smaller width than the
other?  In that case, we need to insert some padding (i.e.,
some spaces) around each of the lines in the narrower image
to match the width of the other image.  There are many ways
to accomplish this, for example, by adding space on the
left, adding space on the right, or on both sides to create
a centering effect.  We will start by describing how these
different kinds of padding can be applied to individual
lines, and then extend those operations to work on complete
images.

There are several ways to approach these tasks.  We could
start by calculating the length of the string that is to be
padded and then figuring out how many extra spaces should be
added (or taken away).  However, it easy to add padding on
the right without calculating the string length: just add an
arbitrarily long list of spaces to the end of the original
string and then truncate to the desired length using take:

> rpad    :: Int -> String -> String
> rpad n s = take n (s ++ repeat ' ')

Padding on the left can also be described in terms of rpad
if we start by reversing the original string, add padding
(on the right), and then reverse again (so that the padding
is on the left):

> lpad    :: Int -> String -> String
> lpad n s = reverse (rpad n (reverse s))

For centering, we can start by breaking the string in to two
halves: pick a "split" point, and then break the original
string s in to two pieces: take split s and drop split s.
After that, we just need to add padding on the left of the
first half, and padding on the right of the second half, and
then put the two half strings back together using ++:

> cpad    :: Int -> String -> String
> cpad n s = lpad lw (take split s) ++ rpad (n-lw) (drop split s)
>            where split = length s `div` 2
>                  lw    = n `div` 2

The trickiest detail here is in deciding how much padding to
put on the left and how much to put on the right of the
centered string.  Because we are limited to integer values
(i.e., whole characters), we will not always be able to
assume two equally sized pieces.  I opted instead to use  lw
= n `div` 2  for the width of the left portion, and then
calculated the width of the right portion using (n-lw)
ensuring that the widths of the two halves always add up to
n, regardless of whether n is even or odd.

Now we can build a function for stacking any two pictures on
top of one another, using one of the above padding functions
(or, in fact, any other padding function, pad, of type Int
-> String -> String) to align the input picture with the
smallest width, as necessary.  To help keep track of the
details, the following code uses the prefixes "t" and "b"
for the variable names here to track which ones are
referring to the "t"op picture, and which are referring to
the "b"ottom picture.

> vertical :: (Int -> String -> String) -> Pic -> Pic -> Pic
> vertical pad (Pic th tw timg) (Pic bh bw bimg)
>   | tw > bw   = Pic h tw (timg ++ adjust tw bimg)
>   | tw < bw   = Pic h bw (adjust bw timg ++ bimg)
>   | otherwise = Pic h tw (timg ++ bimg)
>     where h        = th + bh
>           adjust w = map (pad w)

With that done, the three picture combining functions are
very easy to define as particular instances of the vertical
function:

> infixr `left`, `right`, `center`

> left, right, center :: Pic -> Pic -> Pic
> left   = vertical rpad
> right  = vertical lpad
> center = vertical cpad

What it we want to build a new picture by placing two
smaller pictures together side by side (i.e., horizontally)
instead of vertically as we've done with left, right, and
center?  The same general approach works here too: we'll
provide some padding functions that can be used to add extra
blank rows to a single picture, and then use padding to
combine any two pictures into a single unit by padding the
one with the smaller height to match the one with the larger
height.  Specifically, we can add padding at the top of a
picture, at the bottom, or on both top and bottom so that it
is centered in the middle.  The following three functions
are conceptually very similar to the definitions that we
gave for lpad, rpad, and cpad, but they all require an extra
parameter, w, to specify the width of any blank lines that
are added in the padding process.

> tpad      :: Int -> Int -> [String] -> [String]
> tpad h w s = reverse (bpad h w (reverse s))

> bpad      :: Int -> Int -> [String] -> [String]
> bpad h w s = take h (s ++ repeat (replicate w ' '))

> mpad      :: Int -> Int -> [String] -> [String]
> mpad h w s = tpad th w (take split s) ++ bpad (h-th) w (drop split s)
>               where split = length s `div` 2
>                     th    = h `div` 2

Now we can use the same general idea as before to define a
general function, horiz, for combining images horizontally,
and then instantiate this in three different ways to
implement the top, bottom, and middle operators:

> infixr `top`, `bottom`, `middle`

> top, bottom, middle :: Pic -> Pic -> Pic
> top    = horiz bpad
> bottom = horiz tpad
> middle = horiz mpad

> horiz :: (Int -> Int -> [String] -> [String]) -> Pic -> Pic -> Pic
> horiz pad (Pic lh lw limg) (Pic rh rw rimg)
>   | lh > rh   = Pic lh w (zipWith (++) limg (pad lh rw rimg))
>   | lh < rh   = Pic rh w (zipWith (++) (pad rh lw limg) rimg)
>   | otherwise = Pic lh w (zipWith (++) limg rimg)
>     where w        = lw + rw

Note that we use zipWith (++) here instead of the ++
operator that was used for the vertical combinations.  This
allows us to join the corresponding pairs of lines from each
of the two images together, as is necessary for a horizontal
combination, instead of placing one on top of the other for
a vertical combination.

-----------------------------------------------------------
Packing a list of multiple pictures within a specified width
to form a larger, composite picture.  It will not be
possible to meet the goal if any of the pictures is already
too wide to fit within the expected limit.  If that happens,
then we output the wide picture by itself, overflowing the
margings, and continue to whatever comes next.

> pack         :: Int -> Int -> [Pic] -> Pic
> pack s w []     = empty
> pack s w (p:ps)
>  = case select (w - width p) [p] ps of
>      (ps, []) -> horiz ps
>      (ps, qs) -> horiz ps `center` vstrut 2 `center` pack s w qs
>    where select r ps (q:qs)
>            | r'>=0 = select r' (q:ps) qs
>              where r' = r - (width q + s)
>          select r ps qs = (ps, qs)
>          horiz = foldr1 top . intersperse (hstrut s) . reverse

-----------------------------------------------------------
An anchored picture is a pair that captures a picture and
column value that represents the position of an anchor
column within the picture.

> type APic = (Pic, Int)

> centerstring  :: String -> APic
> centerstring s = (string s, length s `div` 2)

> above         :: APic -> APic -> APic
> (tp, ta) `above` (bp, ba)
>    | ba == ta  = (tp `left` bp,                        ta)
>    | ta >  ba  = (tp `left` (hstrut (ta-ba) `top` bp), ta)
>    | otherwise = ((hstrut (ba-ta) `top` tp) `left` bp, ba)

-----------------------------------------------------------
Our next task is to provide an algorithm for drawing
pictures of trees.  But what exactly do we mean by a "tree"?
For the purposes of this code, all we need, for any given
tree node, is a way to find its label (a simple string to
describe the node) and a list of its subtrees (which could
be empty if the tree in question is a leaf node).

> class Tree t where
>   label    :: t -> String
>   subtrees :: t -> [t]

We'll provide some quick tools for displaying an arbitrary
tree value on the console:

> draw :: Tree t => t -> IO ()
> draw  = putStr . show . treeToPic

> treeToPic  :: Tree t => t -> Pic
> treeToPic   = fst . treeToAPic

We create anchored pictures for trees so that we can attach
a label (in the form of another anchored picture) that is
appropriately aligned/centered with the picture of the tree.

> treeToAPic :: Tree t => t -> APic
> treeToAPic t
>  = let lpic = centerstring (label t)
>        vbar = centerstring "|"
>    in case map treeToAPic (subtrees t) of
>      []  -> lpic
>      [t] -> lpic `above` vbar `above` t
>      ts  -> lpic {-`above` vbar-} `above` spanchildren ts

Drawing the children is not really that difficult, but it
does take some work to get everything in place.  Note, in
particular, that we choose the anchor of the subtree picture
to be the average of the anchors for each of the subtrees,
with a horizontal bar over the top of the list from left to
right.

> spanchildren   :: [APic] -> APic
> spanchildren ts = (foldr1 top pics, total `div` length ts)
>  where
>    total = sum (zipWith (+)
>                    (map snd ts)
>                    (scanl (+) 0 (map width pics)))
>    pics  = addSpan ts
>    addSpan (t:ts) = leftfork t : rest ts
>    rest    [t]    = [rightfork t]
>    rest    (t:ts) = midfork t : rest ts

The following three functions handle the details of drawing
pictures for the leftmost, middle, and rightmost children in
a larger tree:

>    leftfork, midfork, rightfork :: APic -> Pic
>    leftfork (p, a)
>      = (hstrut a `top` (string ('.':replicate (width p -a) '-')
>                         `left` string "|"))
>        `left` p

>    midfork (p, a)
>      = string (replicate (1+width p) '-')
>        `left` (hstrut a `top` string "|")
>        `left` p

>    rightfork (p,a)
>      = string (replicate a '-' ++ ".")
>        `left` (hstrut a `top` string "|")
>        `left` p

We will provide two operators for displaying a list of trees
as a picture.  The first lists each tree, one after the
other, adding a numeric label (starting at 1) to each one.

> listTrees :: Tree t => [t] -> IO ()
> listTrees  = putStrLn
>            . unlines
>            . zipWith (\n p -> show (string (show n ++") ") `top` p)) [1..]
>            . map treeToPic

The second packs as many pictures as it can into a single
picture, attempting to limit the width to some fixed number
of columns, as specified by the second argument to the pack
function in the code below.

> picTrees :: Tree t => [t] -> Pic
> picTrees  = pack 4 64
>           . map fst
>           . zipWith (\n p -> p `above` (vstrut 1, 0)
>                              `above` centerstring ("(" ++ show n ++ ")"))
>                     [1..]
>           . map treeToAPic

-----------------------------------------------------------
Our final task is to construct a utility for displaying a
table of individual pictures as a single image.  Some work
is required here to arranging for all of the entries in each
column to have the same width, and for all of the entries in
any given row to have the same height.  There's a lot to
unpack here in this short chunk of code, so I wouldn't
suggest worrying too much about this if you are new to
Haskell!

> table :: [[Pic]] -> Pic
> table rows = foldr1 left (intersperse hsep (map (vjoin . pad) rows))
>  where
>   widths = map (maximum . map width) (transpose rows)
>   pad    = zipWith (\w p -> hstrut w `center` p) widths
>   hsep   = string (concat (intersperse "-+-" hbars))
>   hbars  = [ replicate w '-' | w <- widths ]
>   vjoin row = foldr1 top (intersperse vsep row)
>    where vsep     = vstr (maximum (map height row)) " | "
>          vstr n s = Pic n (length s) (replicate n s)

-----------------------------------------------------------
