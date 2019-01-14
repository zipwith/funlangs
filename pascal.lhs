
> firstrow   = [1]
> next row   = zipWith (+) ([0]++row) (row++[0])
> triangle n = take n (iterate next firstrow)

> pascal     = putStr . unlines . layout . map showrow . triangle
> showrow   :: [Integer] -> String
> showrow    = foldr1 (\x y -> x ++ " " ++ y) . map show

> layout ls  = map center ls
>  where len = maximum (map length ls)
>        center s = replicate ((len - length s) `div` 2) ' ' ++ s

                    1
                   1 1
                  1 2 1
                1  3 3  1
              1  4  6  4  1
             1  5 10  10  5  1
           1  6  15  20  15  6  1
         1  7  21  35  35  21  7  1
       1  8  28  56  70  56  28  8  1
     . . . . . . . . . . . . . . . . . .

