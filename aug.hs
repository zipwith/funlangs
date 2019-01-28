-------------------------------------------------------------------------------
-- This file contains Haskell source code for the programs described in:
--
--               Using Types to Parse Natural Language
--
--         Mark P. Jones           Paul Hudak and Sebastian Shaumyan
--     University of Nottingham            Yale University
--      Nottingham, England           New Haven, Connecticut, USA
--
-- Proceedings of the Glasgow Workshop on Functional Programming, July 1995.
-- Published by Springer-Verlag in the Workshops in Computer Science series.
--
-- With some modifications, April 2017.
--
-- This version of the program has been tested using Hugs.  To use with
-- GHCi, uncomment the line marked below.
-------------------------------------------------------------------------------

module AUG where 

--import Prelude hiding(Word)  -- uncomment this line for use in GHCi
import Data.List(transpose)

-- Types and Trees: -----------------------------------------------------------

data Type = T | S | O Type Type   deriving Eq

instance Show Type where
  showsPrec d T       = showString "T"
  showsPrec d S       = showString "S"
  showsPrec d (O x y) = showString "O" . shows x . shows y

type TTree = (Tree,[Type]) 
data Tree  = Atom String | FAp TTree TTree | BAp TTree TTree

tree          :: TTree -> Tree 
tree (tr, ty)  = tr

-- Sentences: -----------------------------------------------------------------

type Sentence = [TTree]

sentence :: String -> Sentence
sentence = map wordToTTree . words
 where wordToTTree w = (Atom w, wordTypes w)

myfriend  = "my friend lives in Boston"
oldfriend = "my old friend who comes from Portland"
long      = "my old friend who comes from Portland thinks that\
           \ the film which he saw today was very interesting"

-- Enumerating Types/Trees: ---------------------------------------------------

ttrees       :: Sentence -> [TTree]
ttrees [t]    = [t]
ttrees ts     = [ t | (ls,rs) <- splits ts, l <- ttrees ls,
                                            r <- ttrees rs,
                                            t <- combine l r ]

splits       :: [a] -> [([a],[a])]
splits ts     = zip (inits ts) (tails ts)

inits        :: [a] -> [[a]]
inits [x]     = []
inits (x:xs)  = map (x:) ([]:inits xs)

tails        :: [a] -> [[a]]
tails [x]     = []
tails (x:xs)  = xs : tails xs

combine      :: TTree -> TTree -> [TTree]
combine l r   = app FAp l r ++ app BAp r l

app          :: (TTree -> TTree -> Tree) -> TTree -> TTree -> [TTree]
app op (a,ts) (b,ss)
              = [ (op (a,[O x y]) (b,[x]), [y]) | (O x y)<-ts, z<-ss, x==z ]

-- A More Sophisticated Algorithm: --------------------------------------------

fastTtrees      :: Sentence -> [TTree]
fastTtrees       = head . head . cache

cache           :: Sentence -> [[[TTree]]]
cache [x]        = [[[x]]]
cache (x:xs)     = [build x (transpose rs)] ++ rs
                   where rs = cache xs

build           :: TTree -> [[[TTree]]] -> [[TTree]]
build a []       = [[a]]
build a (ts:tss) = g (reverse is) ts : is
 where is      = build a tss
       g is ts = [ r | (i,t) <- zip is ts,
                        ti   <- i,
                        tt   <- t,
                        r    <- combine ti tt ]

explain         :: String -> IO ()
explain          = putStr . unlines . map drawTTree . fastTtrees . sentence

-- Drawing trees: -------------------------------------------------------------

type Pic = (Int, Int, Int, [String])

drawTTree    :: TTree -> String
drawTTree tr  = unlines ((show (tree tr) ++ ":\n") : ptr)
 where (_,_,_,ptr)   = tpic tr
       tpic (tr,ty)  = oneAbove (pic tr) (label (show ty))
       pic (Atom w)  = label w
       pic (FAp l r) = sideBySide (tpic l) (tpic r)
       pic (BAp l r) = sideBySide (tpic r) (tpic l)
 
label  :: String -> Pic
label a = (1,  l, c, [ " " ++ a])
 where l = 1 + length a
       c = 1 + l`div`2

sideBySide :: Pic -> Pic -> Pic
sideBySide (hl,wl,cl,pl) (hr,wr,cr,pr) = (h+1,w,c,p)
 where h   = hl `max` hr
       w   = wl + wr
       c   = (cl + wl+cr+1) `div` 2
       p   = zipWith (++) (replicate (h-hl) (replicate wl ' ') ++ pl)
                          (replicate (h-hr) (replicate wr ' ') ++ pr) ++ [tie]
       tie = replicate (cl-1)   ' ' ++ "\\" ++
             replicate (c-cl-1) '_' ++ "_" ++ replicate (cr+wl-c-1) '_' ++
             "/" ++ replicate (wr - cr) ' '

oneAbove :: Pic -> Pic -> Pic
oneAbove (ht,wt,ct,pt) (hb,wb,cb,pb) = (ht+hb, w, c, p)
 where c     = ct `max` cb
       w     = c + ((wt-ct) `max` (wb-cb))
       p     = addMargins (c-ct) ((w+ct)-(wt+c)) pt ++
               addMargins (c-cb) ((w+cb)-(wb+c)) pb

addMargins    :: Int -> Int -> [String] -> [String]
addMargins l r = map (\s -> lm ++ s ++ rm)
 where lm = replicate l ' '
       rm = replicate r ' '

-- A simple lexicon, sufficient for examples in the paper: --------------------

wordTypes  :: Word -> [Type]
wordTypes w = findWord w dictionary

data Dictionary = Nil | Node Word [Type] Dictionary Dictionary
type Word       = String

instance Show Dictionary where
   showsPrec d Nil = id
   showsPrec d (Node w ts l r)
     = shows l .
       showString w . showString " :: " . shows ts . showChar '\n' .
       shows r

addWord         :: Type -> Word -> Dictionary -> Dictionary
addWord t w Nil  = Node w [t] Nil Nil
addWord t w (Node v ts l r)
        | w == v = Node v (t:ts) l r
        | w <  v = Node v ts (addWord t w l) r
        | w >  v = Node v ts l (addWord t w r)

findWord        :: Word -> Dictionary -> [Type]
findWord w Nil   = []
findWord w (Node v ts l r) 
        | w == v = ts
        | w <  v = findWord w l
        | w >  v = findWord w r

vocab       :: [Word] -> Type -> Dictionary -> Dictionary
vocab vs t d = foldr (addWord t) d vs

other      :: [(Word,Type)] -> Dictionary -> Dictionary
other wts d = foldr ($) d [ addWord t w | (w,t) <- wts ]

dictionary :: Dictionary 
dictionary  = vocab nouns   T
            $ vocab itverb  (O T S)
            $ vocab trverb  (O T (O T S))
            $ vocab adj     (O T T)
            $ vocab adverb  (O (O T S) (O T S))
            $ other miscwords
            $ Nil

nouns       = ["hat", "wine", "boy", "girl", "father", "mother",
                "Boston", "I", "friend", "word", "home", "he", "she", "enemy",
                "Moscow", "London", "Oregon", "film", "John", "dog", "Mary",
                "Portland", "city", "cat", "mouse", "Sebastian", "Paul",
                "Mark", "computer", "weather"]
itverb      = ["came", "lives", "comes", "saw", "slept", "runs"]
trverb      = ["knew", "see", "knocked", "thinks", "was", "likes", "loves" ]
adj         = ["the", "my", "his", "her", "old", "ill", "a", "young",
               "exciting", "interesting", "this", "small", "favorite",
               "red", "blue", "brown", "yellow", "green",
               "sunny", "rainy", "windy", "warm", "happy", "sad",
               "big", "small", "friendly", "short", "tall" ]
adverb      = ["home", "late", "early", "soundly", "quickly"]

miscwords   = [("that", ost), ("in", otd1), ("tomorrow", oss),
               ("will", d2), ("down", d2), ("was", oap1),
               ("very", oaa), ("today", d1), ("who", otop1t),
               ("from", otd1), ("which", otop1a), ("is", oap1)]

oss         = O S S
ost         = O S T
a           = O T T
p1          = O T S
p2          = O T p1
p3          = O T p2
d1          = O p1 p1
d2          = O p2 p2
otd1        = O T d1
oaa         = O a a
otop1t      = O T (O p1 T)
oap1        = O a p1
otop1a      = O T (O p1 a)

test1       = "the boy came home late"
test2       = "my friend lives in Boston"
test3       = "tomorrow I will see my old friend"
test4       = "he knew that his mother was ill"
test5       = "he knocked down his enemy"
test6       = "the film was very interesting"
test7       = "my old friend who comes from Moscow"
test8       = "my old friend thinks that the film was exciting"
test9       = "the film which he saw today was very interesting"
test0       = "my old friend who comes from Moscow thinks that the film \
              \which he saw today was very interesting"

-- Miscellaneous utilities: ---------------------------------------------------

instance Show Tree where
  showsPrec d (Atom s)           = showString s
  showsPrec d (FAp t (Atom s,_)) = shows (tree t)  .  showChar ' '    .
                                   showString s
  showsPrec d (FAp t u)          = shows (tree t)  .
                                   showString " (" .
                                   shows (tree u)  .
                                   showChar ')'
  showsPrec d (BAp t (Atom s,_)) = shows (tree t)  .
                                   showChar ' '    .
                                   showString s
  showsPrec d (BAp t u)          = shows (tree t)  .
                                   showString " (" .
                                   shows (tree u)  .
                                   showChar ')'

-------------------------------------------------------------------------------
