module Coord where

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

data CoordD = CoordD Double Double

data CoordI = CoordI Int Int

data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x y) (Coord x1 y1) =
  sqrt $ (x - x1) ** 2 + (y - y1) ** 2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord x1 y1) =
  abs (x - x1) + abs (y - y1)

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) =
  Coord (toDouble x * width + width / 2)
        (toDouble y * width + width / 2)
  where
    toDouble = fromInteger . toInteger

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) =
  Coord (toInt $ (x - width / 2) / width)
        (toInt $ (y - width / 2) / width)
  where
    toInt = fromInteger . round


thrice :: a -> (,,) a a a
thrice x = (,,) x x x

data Shape = Circle Double | Rectangle Double Double
	deriving Show

area :: Shape -> Double
area (Circle r) 			= pi * r ^ 2
area (Rectangle a b)	= a * b

square :: Double -> Shape
square a = Rectangle a a
