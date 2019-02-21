------------------------------------------------------------------------

> module PPM where

Support for rendering Images using basic PPM format outputs.

------------------------------------------------------------------------

> import Image

------------------------------------------------------------------------
PPM Colors:

> type PPMcolor = (Int, Int, Int)
> ppmMax        = 31 :: Int

> black, red, green, blue, white :: PPMcolor
> black         = (0, 0, 0)
> red           = (ppmMax, 0, 0)
> green         = (0, ppmMax, 0)
> blue          = (0, 0, ppmMax)
> white         = (ppmMax, ppmMax, ppmMax)
> gray          = dull white

> dull         :: PPMcolor -> PPMcolor
> dull (r,g,b)  = (r `div` 2, g `div` 2, b `div` 2)

------------------------------------------------------------------------
Rendering and writing images:

> ppmRender    :: Grid PPMcolor -> [String]
> ppmRender g   = ["P3", show w ++ " " ++ show h, show ppmMax]
>                 ++ [ show r ++ " " ++ show g ++ " " ++ show b
>                    | row <- g, (r,g,b) <- row ]
>                 where w = length (head g)
>                       h = length g

> writeImage  :: String -> Grid Point -> Image PPMcolor -> IO ()
> writeImage file points
>   = writeFile (file ++ ".ppm") . unlines . ppmRender . sample points white

------------------------------------------------------------------------
