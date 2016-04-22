module Demo where

fibonacci :: Integer -> Integer
fibonacci (-2) = (-1)
fibonacci n | abs n <= 1 = abs n
            | n > 0 = helper 0 1 n
            | n < 0 = helper 1 (-1) (n + 1)

helper :: Integer -> Integer -> Integer -> Integer
helper x y 0 = x
helper x y n | n > 0 = helper y (x + y) (n - 1)
                      | n < 0 = helper y (x - y) (n + 1)


-- bad one
badFib :: Integer -> Integer
badFib (-2) = (-1)
badFib  0 = 0
badFib n | abs n == 1 = 1
      | n > 0 = badFib (n - 1) + badFib (n - 2)
      | n < 0 = badFib (n + 2) - badFib (n + 1)

factorial :: Integer -> Integer
factorial n | n >= 0	= helper 1 n
			| otherwise = error "n < 0"
	where
		helper acc 0 = acc
		helper acc n = helper (acc * n) (n - 1)
