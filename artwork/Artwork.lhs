------------------------------------------------------------------------

> module Artwork where

Some simple artwork using the Image and PPM libraries.

Compile using:  ghc --make Artwork.lhs -main-is Artwork
Cleanup using:  rm *.o *.hi Artwork

> import Image
> import PPM

------------------------------------------------------------------------
A basic canvas for drawing:

> points = grid (-5,-5) (5,5) 300 300
> --points = grid (-5,-1) (-4,0) 300 300          -- flower1
> --points = grid (-4.5,-1) (-4,-0.5) 300 300     -- flower2
> --points = grid (-4.25,-0.75) (-4,-0.5) 300 300 -- flower3

A complex image comprising several image components with suitable
transformations and combinations:

> image = translate (0,2)
>       $ rotate (pi/4)
>       $ stretch 0.5
>         (translate (2,2) (circle 2 blue)
>          `over`
>          rectangle 4 4 red
>          `over`
>          rotate (pi/3) (translate (-2,-2) (square 4 green))
>          `over`
>          translate (-1,0) (semi black))

------------------------------------------------------------------------
A picture of a flower:

> flower  = circle 0.8 red
>           `over`
>           foldr over stem [ rotate (n*pi/6) petal | n <- [0..11] ]
> stem  = xreflect (translate (-1,0) (rectangle 15 2 green))
> petal = xstretch 2.5 (translate (1,0) (circle 0.7 blue))

------------------------------------------------------------------------
A garden, with a row of flowers over a grassy ground:

> garden = row `over` ground
> ground = translate (0,-2) (rotate (pi/2) (semi green))
> row    = foldr over blank [ translate (x, 0) smallFlower | x <- [-4, -2 .. 4] ]
> smallFlower = stretch 0.2 flower

> main  = writeImage "flower" points garden

------------------------------------------------------------------------
