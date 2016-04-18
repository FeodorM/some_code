module Tasks where

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA n = helper 1 2 3 n
	where
		helper x y z 0 = x
		helper x y z n = helper y z (z + y - 2 * x) (n - 1)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sum 0 (abs x), count 0 (abs x))
	where
		sum s 0 = s
		sum s x = sum (s + mod x 10) (div x 10)

		count 0 0 = 1
		count c 0 = c
		count c x = count (c + 1) (div x 10)

--integration' :: (Double -> Double) -> Double -> Double -> Double
--integration' f a b = helper f a b (abs (b - a) / 1000)
--	where
--		helper f a b limit
--			| abs (b - a) < limit = (f a + f b) * (b - a) / 2
--		  | otherwise = 
--		  	let
--		   			mid = (b + a) / 2
--		  	in helper f a mid limit + 
--		    	 helper f mid b limit


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let delta = abs $ (b - a) / 10000
										in helper f a (a + delta) delta 0 10000
	where
		helper f a b delta acc 0 = acc + (f a + f b) * delta / 2
		helper f a b delta acc n = 
			helper f b (b + delta) delta
				(acc + (f a + f b) * delta / 2) (n - 1)


rootsDiff :: Double -> Double -> Double -> Double
rootsDiff a b c = let
		(x1, x2) = roots a b c
	in x2 - x1

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = (x1, x2) 
	where
		d = sqrt $ b ^ 2 - 4 * a * c
		x1 = (-b - d) / aTwise
		x2 = (-b + d) / aTwise
		aTwise = 2 * a

nTimes :: a -> Int -> [a]
nTimes x 0 = []
nTimes x n = helper x [] n
	where
		helper x xs 1 = x : xs
		helper x xs n = helper x (x : xs) (n - 1)

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems x 	= let
									(begin, rest) = span (== head x) x
								in begin : groupElems rest

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 x  y  [] = sum3 x y [0]
sum3 x  [] z  = sum3 x [0] z
sum3 [] y  z  = sum3 [0] y z
sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =  qsort (filter (< x) xs) ++
								[x] ++
								qsort (filter (>= x) xs)

fibStream :: [Integer]
fibStream = f 0 1 where
	f x y = x : f y (x + y)


data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
  toEnum x = Odd $ toInteger $2 * x + 1
  fromEnum (Odd x) = div (fromInteger x) 2
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)
  enumFrom (Odd x) =
  	map Odd $
  	filter odd $
  	enumFrom (toInteger x)
  enumFromThen (Odd x) (Odd y) =
  	map Odd $
  	filter odd $
  	enumFromThen (toInteger x) (toInteger y)
  enumFromTo (Odd x) (Odd y) =
  	map Odd $
  	filter odd $
  	enumFromTo (toInteger x) (toInteger y)
  enumFromThenTo (Odd x) (Odd y) (Odd z) =
  	map Odd $
  	filter odd $
  	enumFromThenTo (toInteger x) (toInteger y) (toInteger z)



data Bit = Zero | One deriving (Show)
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving Show

add :: Z -> Z -> Z
add z1 z2 = toZ $ fromZ z1 + fromZ z2


fromZ :: Z -> Integer
fromZ (Z s x) | s == Minus = (-1) * f x
							| otherwise  = 				f x
	where
		f = foldr (\x rest -> i x + 2 * rest) 0
		i One = 1
		i _		= 0

toZ :: Integer -> Z
toZ x | x < 0 		= Z Minus $ f $ abs x
			| otherwise = Z Plus  $ f x
	where
		f 0 = []
		f x = i (mod x 2) : f (div x 2)

		i 1 = One
		i 0 = Zero


mul :: Z -> Z -> Z
mul z1 z2 = toZ $ fromZ z1 * fromZ z2


import Data.Char (isDigit)
import Data.Text (breakOn, pack, unpack)
import Data.List (find)


data Error = ParsingError |
			 			 IncompleteDataError |
			 			 IncorrectDataError String
	deriving Show

data Person = Person {
	 firstName :: String,
	 lastName :: String,
	 age :: Int 
} deriving Show

parsePerson :: String -> Either Error Person
parsePerson str = let pairs = map splitNameValue $ lines str in
	case find (\(x, y) -> x == "" || y == "") pairs of
		(Just _)	-> Left ParsingError
		Nothing 	-> 
			case lookup "firstName" pairs of
				Nothing 	-> Left IncompleteDataError
				(Just fn) -> 
					case lookup "lastName" pairs of
						Nothing		-> Left IncompleteDataError
						(Just ln)	-> 
							case lookup "age" pairs of
								Nothing 			-> Left IncompleteDataError
								(Just strAge) ->
									case all isDigit strAge of
										False	-> Left $ IncorrectDataError strAge
										True	-> Right $ Person { firstName = fn, lastName = ln, age = read strAge }


splitNameValue :: String -> (String, String)
splitNameValue str = (unpack x, drop 3 $ unpack y) where
	(x, y) = f str
	f = breakOn (pack " = ") . pack {- --!!! (== " = ") -}


	
data Nat = Zero | Suc Nat
	deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc $ toNat $ x - 1

add :: Nat -> Nat -> Nat
add x y = toNat $ fromNat x + fromNat y

mul :: Nat -> Nat -> Nat
mul x y = toNat $ fromNat x * fromNat y

fac :: Nat -> Nat
fac x = toNat $ f (fromNat x) 1
	where
		f 0 acc = acc
		f n acc = f (n - 1) (n * acc)