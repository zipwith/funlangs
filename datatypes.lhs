> import Prelude hiding(lookup)

Basic algebraic datatypes:

> data Rainbow   = Red | Orange | Yellow
>                | Green | Blue | Indigo | Violet
>                  deriving (Eq, Show, Enum)

> data Shape     = Circle Radius
>                | Polygon [Point]
>                | ModifiedBy Transform Shape
>                  deriving Show

> data Transform = Translate Point
>                | Rotate Angle
>                | Compose Transform Transform
>                  deriving Show

> square = [(0,0), (0,1), (1,1), (1,0)] :: [Point]
> type Point     = (Double, Double)
> type Angle     = Double
> type Radius    = Double

Parameterized datatypes:

> data Pair a b = MkPair a b
>                 deriving Show

> first (MkPair x y) = x

  data Maybe a = Nothing | Just a  --- defined in the prelude

> orElse            :: Maybe a -> a -> a
> Just x  `orElse` y = x
> Nothing `orElse` y = y

Approximating Dynamic Typing:

> data Mixed = AnInt Int
>            | ADouble Double
>            | AList [Mixed]
>--          | AFunction (Mixed -> Mixed)
>              deriving Show

