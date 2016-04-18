module Factor where

factorial :: Integer -> Integer
factorial n | n >= 0	= helper 1 n
			| otherwise = error "n < 0"
	where
		helper acc 0 = acc
		helper acc n = helper (acc * n) (n - 1)

