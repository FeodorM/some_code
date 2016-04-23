module Alg where

a, constP :: Integer
constP = 31
a = fromChar 'a'

fromChar :: Char -> Integer
fromChar = toInteger . fromEnum

alg :: String -> String -> IO ()
alg s t = mapM_ printIf last_h
  where
    pPows = iterate (*constP) 1

    h_s = sum [(fromChar ss - a + 1) * p
      | (ss, p) <- zip (take (length t) s) pPows]

    h = head ht : zipWith (+) ht (tail ht)
      where
        ht = zipWith (*) pPows $ map (\x -> fromChar x - a + 1) t

    new_h = zipWith (-) (drop (length s - 1) h) (0 : h)

    last_h = zip3 new_h [1..] $ map (* h_s) pPows

    printIf (x, i, y)  | x == y    = putStr $ show i ++ " "
                       | otherwise = return ()
