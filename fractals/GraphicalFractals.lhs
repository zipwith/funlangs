> module GraphicalFractals(main) where
> import Fractals
> import Regions
> import GraphicsUtils hiding (Point)
> import Array

Drawing Fractals with Graphical Primitives:

> rgbPalette  :: [RGB]
> rgbPalette   = cols++cols --[ RGB (255-x) x x | x <- [7, 15 .. 255] ]
> cols         = map (colorTable!)
>                    [Black, Cyan, Blue, Green, Magenta, Yellow, Red, White ]

> graphicsWindow    :: Int -> Int -> IO Window
> graphicsWindow w h = openWindow "Composing Fractals" (w*scx, h*scy)

> setPixel          :: Window -> Int -> Int -> RGB -> IO ()
> setPixel w x y c   = drawInWindow w
>                       (withRGB c
>                        (polygon [(x0,y0),(x0,y1),(x1,y1),(x1,y0),(x0,y0)]))
>                      where x0 = scx*x; x1=x0+scx
>                            y0 = scy*y; y1=y0+scy

> rgbRender  :: Grid RGB -> IO ()
> rgbRender g = do w <- graphicsWindow (length (head g)) (length g)
>                  sequence_ [ setPixel w x y c | (row,y) <- zip g [0..],
>                                                 (c,x) <- zip row [0..] ]
>                  getKey w
>                  closeWindow w

> figure3left  = draw points mandelbrot rgbPalette rgbRender
>                where points = grid (-2.25, -1.5) (0.75, 1.5) 240 160
>
> figure3right = draw points (julia (0.32,0.043)) rgbPalette rgbRender
>                where points = grid (-1.5, -1.5) (1.5, 1.5) 240 160

> scx = 2 :: Int
> scy = 2 :: Int

> main = runGraphics figure3left

