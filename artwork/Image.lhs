------------------------------------------------------------------------

> module Image where

A demonstration of the use of functions as a representation for
arbitrary precision, unbounded images.

------------------------------------------------------------------------
Image type:

> type Image color = Point -> Maybe color
> type Point       = (Float, Float)

------------------------------------------------------------------------
Basic shapes:

> blank              :: Image color
> blank               = \(u,v) -> Nothing

Rectangle, bottom left corner at origin with specified height and width:

> rectangle          :: Float -> Float -> color -> Image color
> rectangle h w col   = \(u,v) -> if u>=0 && u<=w && v>=0 && v<=h
>                                   then Just col
>                                   else Nothing

> square             :: Float -> color -> Image color
> square w            = rectangle w w

Circle, centered at the origin, with specified radius:

> circle             :: Float -> color -> Image color
> circle r col        = \(u,v) -> if u*u + v*v <= rsquared
>                                  then Just col else Nothing
>                       where rsquared = r * r

Semi plane for positive x:

> semi               :: color -> Image color
> semi col            = \(u,v) -> if u>=0 then Just col else Nothing

------------------------------------------------------------------------
Transformations:

> translate           :: Point -> Image color -> Image color
> translate (x, y) src = \(u,v) -> src (u-x, v-y)

> stretch             :: Float -> Image color -> Image color
> stretch m src        = \(u,v) -> src (u/m, v/m)

> xstretch            :: Float -> Image color -> Image color
> xstretch m src       = \(u,v) -> src (u/m, v)

> ystretch            :: Float -> Image color -> Image color
> ystretch m src       = \(u,v) -> src (u, v/m)

> xreflect, yreflect  :: Image color -> Image color
> xreflect img         = \(u,v) -> img (u, -v)
> yreflect img         = \(u,v) -> img (-u, v)

> rotate              :: Float -> Image color -> Image color
> rotate theta src     = \(u,v) -> src (c*u-s*v, s*u+c*v)
>                        where c = cos theta
>                              s = sin theta

------------------------------------------------------------------------
Combining forms:

> over :: Image color -> Image color -> Image color
> over top bot = \p -> case top p of
>                        Nothing -> bot p
>                        r       -> r

> mask :: Image color -> Image color -> Image color
> mask pos neg = \p -> case neg p of
>                        Just _  -> Nothing
>                        Nothing -> pos p

------------------------------------------------------------------------
Grids and Sampling:

> type Grid a = [[a]]

> grid :: Point -> Point -> Int -> Int -> Grid Point
> grid (xmin,ymin) (xmax,ymax) c r
>  = [[ (x,y) | x <- for c xmin xmax ] | y <- for r ymin ymax ]
>    where for          :: Int -> Float -> Float -> [Float]
>          for n min max = take n [min, min+delta_ ..]
>                          where delta_ = (max-min) / fromIntegral (n-1)

> sample :: Grid Point -> color -> Image color -> Grid color
> sample points bg image
>   = reverse (map (map (maybe bg id . image)) points)

------------------------------------------------------------------------
