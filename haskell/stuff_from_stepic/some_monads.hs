module SomeMonads where

import Data.Monoid
import Control.Monad (replicateM_)

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

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k = State $ \s ->
    let (a, s') = runState m s
        m' = k a
    in runState m' s'

execState :: State s a -> s -> s
execState m = snd . runState m

evalState :: State s a -> s -> a
evalState m = fst . runState m

put :: s -> State s ()
put s = State $ const ((), s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \s -> (a, s `mappend` l)  where (a, l) = runWriter m

tick :: State Int Int
tick = do
  n <- get
  put $ n + 1
  return n

succ' :: Int -> Int
succ' = execState tick

plus :: Int -> Int -> Int
plus n = execState $ replicateM_ n tick

fibStep :: State (Integer, Integer) ()
fibStep = do
  (x, y) <- get
  put (y, x + y)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM_ n m

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState ff (tree, 1) where
  ff :: State (Tree (), Integer) (Tree Integer)
  ff = do
    (tr, i) <- get
    put (tr, i + 1)
    case tr of
      (Leaf ())     -> return $ Leaf i
      (Fork l () r) -> do
        ll <- runState ff (l, i)
        (_, st) <- get
        rr <- runState ff (r, st)
        modify (\(t, v) -> (t, v + 1))
        return $ Fork ll st rr

myTree :: Tree ()
myTree = Fork (Fork (Leaf ())
                    ()
                    (Fork (Leaf ())
                          ()
                          (Leaf ())))
               ()
               (Fork (Leaf ())
                     ()
                     (Leaf ()))

main :: IO ()
main = print $ runState (writerToState $ tell "world") "hello "

-- let (ll, (_, new_i_l)) = runState ff (l, i)
-- let (rr, (_, new_i_r)) = runState ff (r, new_i_l + 1)
-- modify (\(t, v) -> (t, v + 1))
