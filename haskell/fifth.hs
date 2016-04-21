module Fifth where

import Data.Functor ((<$>))

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
  fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)

data GeomPrimitive a = Point (Point3D a) |
                       LineSegment (Point3D a) (Point3D a)
                        deriving (Show)

instance Functor GeomPrimitive where
  fmap f (Point a)          = Point $ fmap f a
  fmap f (LineSegment a b)  = LineSegment (fmap f a) (fmap f b)

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf a)       = Leaf $ fmap f a
  fmap f (Branch l a r) = Branch (fmap f l) (fmap f a) (fmap f r)

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry k v) = Entry k $ f v

instance Functor (Map k1 k2) where
  fmap f (Map list) = Map $ map (fmap f) list

data Log a = Log [String] a deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s a = Log [s] (f a)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a f1 f2 = Log (l1 ++ l2) c where
  (Log l1 b) = f1 a
  (Log l2 c) = f2 b

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg a) f = Log (msg ++ s) b
  where
    (Log s b) = f a

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a = foldl (>>=) (Log [] a)

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Monad Identity where
  return = Identity
  Identity x >>= k = k x

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ = Identity . succ

goWrap0 =
  wrap'n'succ 3 >>=
  wrap'n'succ >>=
  wrap'n'succ >>=
  return

goWrap1 =
  wrap'n'succ 3 >>= (\x ->
  wrap'n'succ x >>= (\y ->
  wrap'n'succ y >>= (\z ->
  return z)))

goWrap2 =
  wrap'n'succ 3 >>= (\x ->
  wrap'n'succ x >>= (\y ->
  wrap'n'succ y >>= (\z ->
  return (x, y, z))))

goWrap3 =
  wrap'n'succ 3 >>= \x ->
  wrap'n'succ x >>= \y ->
  wrap'n'succ y >>
  return (x + y)

goWrap4 =
  let i = 3 in
  wrap'n'succ i >>= \x ->
  wrap'n'succ x >>= \y ->
  wrap'n'succ y >>
  return (i, x + y)

goWrap5 = do
  let i = 3
  x <- wrap'n'succ i
  y <- wrap'n'succ x
  wrap'n'succ y
  return (i, x + y)

-- instance Functor SomeType where
--     fmap f x = x >>= (return . f)

bla :: [Integer]
bla = (+2) <$> [1..10]
