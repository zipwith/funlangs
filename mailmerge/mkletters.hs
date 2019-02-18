import Data.Char
import System.Directory

main :: IO ()
main  = do people <- fmap lines (readFile "people")
           blank  <- readFile "blank"
           createDirectoryIfMissing False outdir
           mapM_ (mkLetter blank) people

outdir :: String
outdir  = "letters"

mkLetter :: String -> String -> IO ()
mkLetter text name
  = writeFile (outdir ++ "/" ++ filter (not . isSpace) name ++ ".txt")
              (replace name text)

replace :: String -> String -> String
replace name ""   = ""
replace name text
 = case span ('X'/=) text of
     (nonxs, cs) ->
       nonxs ++
       case span ('X'==) cs of
         (xs, ys) | n < 5     -> xs ++ rest
                  | otherwise -> name ++ rest
                                 where n    = length xs
                                       rest = replace name ys

