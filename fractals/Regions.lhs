> module Regions where
> import Fractals

Some nice regions of the Mandelbrot Set:

> region1 = grid (-2.25, -1.5) (0.75, 1.5)

> mandregions = [region1,
>                grid ((-1.15), 0.19) ((-0.75), 0.39),
>                grid ((-0.19920),1.01480) ((-0.12954),1.06707),
>                grid ((-0.95),0.23333) ((-0.88333),0.3),
>                grid ((-0.713),0.49216) ((-0.4082),0.71429) ]

Some Julia Set regions:

> region2 = grid (-1.5, -1.5) (1.5, 1.5)

> julregions  = [region2,
>                grid ((-0.19920),1.01480) ((-0.12954),1.06707),
>                grid (-0.15,0.60) (0.60,1.166),
>                grid (0.025, 0.375) (0.883, 1.021875)]
