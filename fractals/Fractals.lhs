> module Fractals where

Basic Algorithm for Calculating Fractal Images:

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

> grid :: Point -> Point -> Int -> Int -> Grid Point
> grid (xmin,ymin) (xmax,ymax) c r
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

Julia Set Fractals:

> julia  :: Point -> Point -> [Point]
> julia c = iterate (next c)

> juliaFrac = julia (0.32,0.043)

