> module Main(main) where
> import Fractals
> import Regions

Drawing Fractals with ppm output:

> type PPMcolor = (Int, Int, Int)
> ppmPalette :: [PPMcolor]
> ppmPalette  = [ (((2*i) `mod` (ppmMax+1)), i, ppmMax-i) | i<-[0..ppmMax] ]
> ppmMax      = 31 :: Int

> ppmRender  :: Grid PPMcolor -> [String]
> ppmRender g = ["P3", show w ++ " " ++ show h, show ppmMax]
>               ++ [ show r ++ " " ++ show g ++ " " ++ show b
>                  | row <- g, (r,g,b) <- row ]
>               where w = length (head g)
>                     h = length g

> ppmFrac :: String -> (Point -> [Point]) -> Grid Point -> IO ()
> ppmFrac file frac points
>          = writeFile file
>              (unlines
>                (draw points frac ppmPalette ppmRender))

> ppmTest prefix frac regions
>   = sequence_ [ ppmFrac (prefix ++ show n ++ ".ppm") frac (r 1024 768)
>               | (n, r) <- zip [1..] regions ]

> --main = ppmFrac "mand.ppm" mandelbrot (region1 1024 768)
> main = do ppmTest "mand"  mandelbrot mandregions
>           ppmTest "julia" juliaFrac julregions

