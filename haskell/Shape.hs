module Shape where

data Shape = Circle Double | Rectangle Double Double
	deriving Show

area :: Shape -> Double
area (Circle r) 			= pi * r ^ 2
area (Rectangle a b)	= a * b

square :: Double -> Shape
square a = Rectangle a a