module SomeMonads where

import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, log1) = runWriter m
        (y, log2) = runWriter $ k x
    in Writer (y, log1 `mappend` log2)

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd $ runWriter m

evalWriter :: Writer w a -> a
evalWriter m = fst $ runWriter m

tell :: Monoid w => w -> Writer w ()
tell w = writer ((), w)

calc :: (Int -> Int -> Int) -> Int -> Int -> Writer String Int
calc op arg1 arg2 = do
  let res = arg1 `op` arg2
  tell "ok "
  if abs res < 128 then
    return res
  else do
    tell "overflow"
    return res

type Shopping = Writer (Sum Integer, [String]) ()


total :: Shopping -> Integer
total = getSum . fst . execWriter

purchase :: String -> Integer -> Shopping
purchase str int = tell (Sum int, [str])

items :: Shopping -> [String]
items = snd . execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328
