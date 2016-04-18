module Demo where

import Prelude hiding (length, (++), null, last, init, reverse,
						sum, product, maximum, minimum, 
						zip, zip3, unzip, take, drop, splitAt, (!!),
						filter, takeWhile, dropWhile, span, break,
						map, concat, concatMap, and, or, all, any,
						zipWith, repeat, replicate, cycle,
						iterate)

length :: [a] -> Int
length []		= 0
length (_ : xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys 		= ys
(x : xs) ++ ys  = x : xs ++ ys

null :: [a] -> Bool
null [] = True
null _  = False

last :: [a] -> a
last [] 	  = error "last error"
last (x : []) = x
last (_ : xs) = last xs

init :: [a] -> [a]
init [] 	  = error "Init Error"
init [_] 	  = []
init (x : xs) = x : init xs

reverse :: [a] -> [a]
reverse x = helper x []
	where
		helper [] list 		 = list
		helper (x : xs) list = helper xs (x : list)

sum :: (Num a) => [a] -> a
sum x  = helper x 0
	where
		helper [] acc 		= acc
		helper (x : xs) acc = helper xs (x + acc)


product :: (Num a) => [a] -> a
product x  = helper x 1
	where
		helper [] acc 		= acc
		helper (x : xs) acc = helper xs (x * acc)


maximum :: (Ord a) => [a] -> a
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

minimum :: (Ord a) => [a] -> a
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

oddsOnly :: (Integral a) => [a] -> [a]
oddsOnly x = f (reverse x) [] where
	f [] acc		= acc
	f (x : xs) acc	= f xs (if odd x then (x : acc) else acc)

zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _	   _	  = []

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3 xs ys zs
zip3 _		_	   _	  = []

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y) : xys) = 
	let (xs, ys) = unzip xys
	in (x:xs, y:ys)

take :: Int -> [a] -> [a]
take n _ 	  | n <= 0 = []
take _ []	   		   = []
take n (x:xs) 		   = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n x 	  | n <= 0 = x
drop _ [] 	  		   = []
drop n (_:xs) 		   = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

xs 		!! n | n < 0 = error ""
[] 		!! _ 		 = error ""
(x:_)   !! 0 		 = x
(_:xs)	!! n  		 = xs !! (n - 1)


filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
	| p x 			= x : filter p xs
	| otherwise	= filter p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
	| p x 			= x : takeWhile p xs
	| otherwise	= []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
	| p x 			= dropWhile p xs'
	| otherwise	= xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

--bla :: [a] -> [[a]]
--bla = concatMap perms



--perms :: [a] -> [[a]]
--perms [] 			= []
--perms [x]			= [[x]]
--perms (x:xs)	= 
--perms (x:xs)	= map (\xs' -> x:xs') (perms xs)


and, or :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or [] = False
or (x:xs) = x || and xs

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _	[]	_		= []
zipWith _	_ 	[]	= []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 maximum where
	maximum x y z = max (max x y) z

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate :: Int -> a -> [a]
replicate n a = take n $ repeat a

cycle :: [a] -> [a]
cycle [] = undefined
cycle xs = ys where ys = xs ++ ys

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)