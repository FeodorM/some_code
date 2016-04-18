module DataTypes where

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Error _ 		= GT
cmp Warning Error = LT
cmp Warning Warning = EQ
cmp Warning Info = GT
cmp Info Info = EQ
cmp Info _ = LT

lessThanError :: LogLevel -> Bool
lessThanError lvl =
	case cmp lvl Error of 
		LT -> True
		_  -> False


data Point = Pt Double Double deriving Show

origin :: Point
origin = Pt 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Pt x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Pt x1 y1) (Pt x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2


data Result' = Res Int | Success

instance Show Result' where
	show (Res x) 	= "Fail: " ++ show x
	show _				= "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork x =
  case doSomeWork x of
    (_, 0) -> Success
    (_, n) -> Res n