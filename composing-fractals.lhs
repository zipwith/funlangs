> module Fractals where
> import Array

> type Point = (Float, Float)

> next            :: Point -> Point -> Point
> next (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

> mandelbrot   :: Point -> [Point]
> mandelbrot p  = iterate (next p) (0,0)

> fairlyClose      :: Point -> Bool
> fairlyClose (u,v) = (u*u + v*v) < 100

> inMandelbrotSet  :: Point -> Bool
> inMandelbrotSet p = all fairlyClose (mandelbrot p)

> approxTest    :: Int -> Point -> Bool
> approxTest n p = all fairlyClose (take n (mandelbrot p))

> chooseColor        :: [color] -> [Point] -> color
> chooseColor palette = (palette!!) . length . take n . takeWhile fairlyClose
>                       where n = length palette - 1

> type Image color = Point -> color

> fracImage                :: (Point -> [Point]) -> [color] -> Image color
> fracImage fractal palette = chooseColor palette . fractal

> type Grid a = [[a]]

> grid :: Int -> Int -> Point -> Point -> Grid Point
> grid c r (xmin,ymin) (xmax,ymax)
>  = [[ (x,y) | x <- for c xmin xmax ] | y <- for r ymin ymax ]

> for          :: Int -> Float -> Float -> [Float]
> for n min max = take n [min, min+delta_ ..]
>                 where delta_ = (max-min) / fromIntegral (n-1)

> sample             :: Grid Point -> Image color -> Grid color
> sample points image = map (map image) points

> draw :: Grid Point
>           -> (Point -> [Point])
>             -> [color]
>               -> (Grid color -> image)
>                 -> image
> draw points fractal palette render
>    = render (sample points (fracImage fractal palette))

> charPalette :: [Char]
> charPalette  = "    ,.`\"~:;o-!|?/<>X+={^O#%&@8*$"

> charRender :: Grid Char -> IO ()
> charRender  = putStr . unlines

> figure1 = draw points mandelbrot charPalette charRender
>           where points = grid 79 37 (-2.25, -1.5) (0.75, 1.5)

> julia  :: Point -> Point -> [Point]
> julia c = iterate (next c)

> figure2 = draw points (julia (0.32,0.043)) charPalette charRender
>           where points = grid 79 37 (-1.5, -1.5) (1.5, 1.5)

> mand p q = draw (grid 79 37 p q) mandelbrot charPalette charRender

> demo6 = mand ((-1.15), 0.19) ((-0.75), 0.39)
> map27 = mand ((-0.19920),1.01480) ((-0.12954),1.06707)
> map29 = mand ((-0.95),0.23333) ((-0.88333),0.3)
> map30 = mand ((-0.713),0.49216) ((-0.4082),0.71429)


