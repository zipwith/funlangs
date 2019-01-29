This file will contain code for solving "The Countdown Problem",
built up in class on January 29, 2019, but heavily based on the
implementation in Chapter 9, "Programming in Haskell (2nd ed)"
by Graham Hutton.

> import Data.List

> data Expr = Val Int
>           | App Op Expr Expr
>             deriving Show

> data Op   = Add | Sub | Mul | Div
>             deriving Show

> example = App Add (Val 1) (Val 2)
> ex2     = App Mul example example
> ex3     = App Sub (Val 3) (Val 4)

> showExpr              :: Expr -> String
> showExpr (Val n)       = show n
> showExpr (App op l r)  = parenExpr l ++ showOp op ++ parenExpr r
>  where
>   parenExpr (Val n)      = show n
>   parenExpr e            = "(" ++ showExpr e ++ ")"
>
>   showOp                :: Op -> String
>   showOp Add             = "+"
>   showOp Sub             = "-"
>   showOp Mul             = "*"
>   showOp Div             = "/"

Evaluation:

> evalExpr             :: Expr -> [Int]
> evalExpr (Val n)      = [ n ]
> evalExpr (App op l r) = [ v | lv <- evalExpr l,
>                               rv <- evalExpr r,
>                               v  <- evalOp op lv rv ]

> evalOp               :: Op -> Int -> Int -> [Int]
> evalOp Add x y        = [ x + y     | x >= y ]
> evalOp Sub x y        = [ x - y     | x > y ]
> evalOp Mul x y        = [ x * y     | x >= y, x/=1, y/=1 ]
> evalOp Div x y        = [ x `div` y | y>1, (x `mod` y)==0 ]

Enumerating expressions:

> exprs    :: [Int] -> [Expr]
> exprs []  = []
> exprs [n] = [Val n]
> exprs ns  = [ App op l r  | (ls, rs) <- split ns,
>                             l        <- exprs ls,
>                             r        <- exprs rs,
>                             op       <- ops ]

> ops :: [Op]
> ops  = [Add, Sub, Mul, Div]

> split       :: [a] -> [([a], [a])]
> split []     = []
> split [n]    = []
> split (n:ns) = ([n], ns) : [ (n:ls, rs) | (ls, rs) <- split ns ]

   Main> putStr $ unlines $ map showExpr $ exprs [1,2,3]
   1+(2+3)
   1-(2+3)
   ...
   Main>

Combinatorial Functions:

> subs       :: [a] -> [[a]]
> subs []     = [[]]
> subs (n:ns) = subs ns ++ map (n:) (subs ns)

> perms      :: [a] -> [[a]]
> perms []    = [[]]
> perms (n:ns)= concat (map (interleave n) (perms ns))

perms [2,3] = [ [2,3], [3,2] ]

[1,2,3], [1,3,2]
[2,1,3], [3,1,2]
[2,3,1], [3,2,1]

> interleave         :: a -> [a] -> [[a]]
> interleave n []     = [[n]]
> interleave n (m:ms) = (n:m:ms) : map (m:) (interleave n ms)

> choices :: [a] -> [[a]]
> choices  = concat . map perms . subs

Solving puzzles:

> type Puzzle = ([Int], Int)

> solutions        :: Puzzle -> [Expr]
> solutions (ns, t) = [ e  | cs <- choices ns,
>                            e  <- exprs cs,
>                            v  <- evalExpr e,
>                            v == t ]

Enumerating results:

> type Result = (Expr, Int)

> results    :: [Int] -> [Result]
> results []  = []
> results [n] = [(Val n, n)]
> results ns  = [(App op l r, v) | (ls, rs) <- split ns,
>                                  (l, lv)  <- results ls,
>                                  (r, rv)  <- results rs,
>                                  op       <- ops,
>                                  v        <- evalOp op lv rv ]

> solve        :: Puzzle -> [Expr]
> solve (ns, t) = [ e | cs     <- choices ns,
>                       (e, v) <- results cs,
>                       v == t ]

> screenshot :: Puzzle
> screenshot  = ([10, 9, 3, 9, 2, 6], 670)

   Main> map showExpr $ solve screenshot
   ["((((9+3)*9)+2)*6)+10","((((9+3)*9)+2)*6)+10"]
   Main> 

