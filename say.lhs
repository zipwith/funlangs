> import Data.List
> import Data.Char

> say  :: String -> String
> say   = ('\n':)
>       . unlines
>       . map (foldr1 (\xs ys->xs++"  "++ys))
>       . transpose
>       . map picChar

> picChar    :: Char -> [String]
> picChar 'a' = [ "  A  ", " A A ", "AAAAA", "A   A", "A   A" ]
> picChar 'b' = [ "BBBB ", "B   B", "BBBB ", "B   B", "BBBB " ]
> picChar 'c' = [ " CCCC", "C    ", "C    ", "C    ", " CCCC" ]
> picChar 'd' = [ "DDDD ", "D   D", "D   D", "D   D", "DDDD " ]
> picChar 'e' = [ "EEEEE", "E    ", "EEEEE", "E    ", "EEEEE" ]
> picChar 'f' = [ "FFFFF", "F    ", "FFFF ", "F    ", "F    " ]
> picChar 'g' = [ " GGGG", "G    ", "G  GG", "G   G", " GGG " ]
> picChar 'h' = [ "H   H", "H   H", "HHHHH", "H   H", "H   H" ]
> picChar 'i' = [ "IIIII", "  I  ", "  I  ", "  I  ", "IIIII" ]
> picChar 'j' = [ "JJJJJ", "   J ", "   J ", "J  J ", " JJ  " ]
> picChar 'k' = [ "K   K", "K  K ", "KKK  ", "K  K ", "K   K" ]
> picChar 'l' = [ "L    ", "L    ", "L    ", "L    ", "LLLLL" ]
> picChar 'm' = [ "M   M", "MM MM", "M M M", "M   M", "M   M" ]
> picChar 'n' = [ "N   N", "NN  N", "N N N", "N  NN", "N   N" ]
> picChar 'o' = [ " OOO ", "O   O", "O   O", "O   O", " OOO " ]
> picChar 'p' = [ "PPPP ", "P   P", "PPPP ", "P    ", "P    " ]
> picChar 'q' = [ " QQQ ", "Q   Q", "Q   Q", "Q  Q ", " QQ Q" ]
> picChar 'r' = [ "RRRR ", "R   R", "RRR  ", "R  R ", "R   R" ]
> picChar 's' = [ " SSSS", "S    ", " SSS ", "    S", "SSSS " ]
> picChar 't' = [ "TTTTT", "  T  ", "  T  ", "  T  ", "  T  " ]
> picChar 'u' = [ "U   U", "U   U", "U   U", "U   U", " UUU " ]
> picChar 'v' = [ "V   V", "V   V", "V   V", " V V ", "  V  " ]
> picChar 'w' = [ "W   W", "W   W", "W W W", "WW WW", "W   W" ]
> picChar 'x' = [ "X   X", " X X ", "  X  ", " X X ", "X   X" ]
> picChar 'y' = [ "Y   Y", " Y Y ", "  Y  ", "  Y  ", "  Y  " ]
> picChar 'z' = [ "ZZZZZ", "   Z ", "  Z  ", " Z   ", "ZZZZZ" ]
> picChar '.' = [ "     ", "     ", "     ", " ..  ", " ..  " ]
> picChar ',' = [ "     ", "     ", "     ", " ,,  ", "  ,  " ]
> picChar '!' = [ " !!  ", " !!  ", " !!  ", "     ", " !!  " ]
> picChar ' ' = [ "     ", "     ", "     ", "     ", "     " ]
> picChar '?' = [ " ??? ", "?   ?", "   ? ", "  ?  ", "  .  " ]
> picChar '(' = [ "   ( ", "  (  ", "  (  ", "  (  ", "   ( " ]
> picChar ')' = [ " )   ", "  )  ", "  )  ", "  )  ", " )   " ]
> picChar ':' = [ "     ", "  :: ", "     ", "  :: ", "     " ]
> picChar '-' = [ "     ", "     ", "-----", "     ", "     " ]
> picChar c
>  |isUpper c = picChar (toLower c)
> picChar c   = picChar '?'

Making bigger banners:

> bigSay  :: String -> String
> bigSay   = concat . map say . lines . say

Making crazy large banners:

> hugeSay :: String -> String
> hugeSay  = concat . map say . lines . bigSay

> sayHuge :: String -> String
> sayHuge  = concat . map bigSay . lines . say

Do you think that hugeSay and sayHuge are the same?
Do you think that you could prove they are the same?

