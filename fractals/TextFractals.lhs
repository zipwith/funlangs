> module Main(main) where
> import Fractals
> import Regions

Drawing Fractals with Characters:

> charPalette :: [Char]
> charPalette  = "    ,.`\"~:;o-!|?/<>X+={^O#%&@8*$"

> charRender :: Grid Char -> IO ()
> charRender  = putStr . unlines

> figure1 = draw points mandelbrot charPalette charRender
>           where points = region1 79 37

> figure2 = draw points juliaFrac charPalette charRender
>           where points = region2 79 37

> main = figure1
