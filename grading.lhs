Your total score is made up from the combination of homework,
project, and final scores:

> score       :: Double
> score        = homework + project + final

The maximum possible points for each homework are as follows:

> hwmaximums  :: [Double]
> hwmaximums   = [50, 100, 50, 100, 100, 100]

And all homeworks are equally weighted, which means that the
points for each one individually is given by:

> onehomework :: Double
> onehomework  = 40 / fromIntegral (length hwmaximums)

So if your scores on the homework assignments are as follows:

> hwscores    :: [Double]
> hwscores     = [25, 63, 32,  50,  50,  50]

Then your overall grade for the homework would be:

> homework    :: Double
> homework     = onehomework * sum (zipWith (/) hwscores hwmaximums)

To complete the calculation, you can add in estimates for your
project and final scores:

> project     :: Double
> project      = 30   -- out of 50

> final       :: Double
> final        = 10   -- out of 10

