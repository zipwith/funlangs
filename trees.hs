-- You will need some special flags to load this in to hugs/ghci,
-- such as:
--
--   ghci -XFlexibleInstances trees.hs
--   hugs -98 trees.hs
--

{-# LANGUAGE FlexibleInstances #-}

-- Basic binary trees:

data BinTree a   = Leaf a
                 | BinTree a :^: BinTree a
                   deriving Show

example :: BinTree Int
example  = l :^: r
 where l = p :^: q
       r = s :^: t
       p = Leaf 1 :^: t
       q = s :^: Leaf 2
       s = Leaf 3 :^: Leaf 4
       t = Leaf 5 :^: Leaf 6

mapTree            :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf x)  = Leaf (f x)
mapTree f (l :^: r) = mapTree f l :^: mapTree f r

-- Some other kinds of tree structure:

data LabTree l a = Tip a
                 | LFork l (LabTree l a) (LabTree l a)

data STree a     = Empty
                 | Split a (STree a) (STree a)

data RoseTree a  = Node a [RoseTree a]

data Expr        = Var String
                 | IntLit Int
                 | Plus Expr Expr
                 | Mult Expr Expr

-- What does it mean to be a tree?

class Tree t where
  subtrees :: t -> [t]

instance Tree (BinTree a) where
  subtrees (Leaf x)   = []
  subtrees (l :^: r)  = [l, r]

instance Tree (LabTree l a) where
  subtrees (Tip a)       = []
  subtrees (LFork s l r) = [l, r]

instance Tree (STree a) where
  subtrees Empty = []
  subtrees (Split s l r) = [l, r]

instance Tree (RoseTree a) where
  subtrees (Node x cs) = cs

instance Tree Expr where
  subtrees (Var s)    = []
  subtrees (IntLit n) = []
  subtrees (Plus l r) = [l, r]
  subtrees (Mult l r) = [l, r]

-- Generic operations on trees:

depth  :: Tree t => t -> Int
depth   = (1+) . foldl max 0 . map depth . subtrees

size   :: Tree t => t -> Int
size    = (1+) . sum . map size . subtrees

paths               :: Tree t => t -> [[t]]
paths t | null br    = [ [t] ]
        | otherwise  = [ t:p | b <- br, p <- paths b ]
          where br = subtrees t

dfs    :: Tree t => t -> [t]
dfs t   = t : concat (map dfs (subtrees t))

-- Labeled trees:

class Tree t => LabeledTree t where
  label :: t -> String

instance LabeledTree (BinTree String) where
  label (Leaf x)   = x
  label (l :^: r)  = ""

instance LabeledTree (LabTree String String) where
  label (Tip a)       = a
  label (LFork s l r) = s

instance LabeledTree (STree String) where
  label Empty         = ""
  label (Split s l r) = s

instance LabeledTree (RoseTree String) where
  label (Node x cs) = x

instance LabeledTree Expr where
  label (Var s)    = s
  label (IntLit n) = show n
  label (Plus l r) = "+"
  label (Mult l r) = "*"

-- Generating dot output:

type Path      = [Int]
type NodeId  = String

showPath      :: Path -> NodeId
showPath p     = "\"" ++ show p ++ "\""

toDot  :: LabeledTree t => t -> IO ()
toDot t = writeFile "tree.dot"
           ("digraph tree {\n"
            ++ semi (nodeTree [] t) ++ "}\n")
 where semi = foldr (\l ls -> l ++ ";\n" ++ ls) ""

       nodeTree    :: LabeledTree t => Path -> t -> [String]
       nodeTree p t
         = [ showPath p ++ " [label=\"" ++ label t ++ "\"]" ]
         ++ concat (zipWith (edgeTree p) [1..] (subtrees t))

       edgeTree      :: LabeledTree t => Path -> Int -> t -> [String]
       edgeTree p n c = [ showPath p ++ " -> " ++ showPath p' ]
                      ++ nodeTree p' c
                        where p' = n : p

-- Maps on trees:

instance Functor BinTree where
  fmap f (Leaf x)   = Leaf (f x)
  fmap f (l :^: r)  = fmap f l :^: fmap f r

instance Functor (LabTree l) where
  fmap f (Tip a)       = Tip (f a)
  fmap f (LFork s l r) = LFork s (fmap f l) (fmap f r)

instance Functor STree where
  fmap f Empty         = Empty
  fmap f (Split s l r) = Split (f s) (fmap f l) (fmap f r)

instance Functor RoseTree where
  fmap f (Node x cs) = Node (f x) (map (fmap f) cs)


