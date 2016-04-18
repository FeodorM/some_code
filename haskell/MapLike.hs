module MapLike where


import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
	empty = ListMap []

	lookup k (ListMap l) = f k l 
		where
			f k [] = Nothing
			f k ((x, y):xs) | k == x 		= Just y
											| otherwise = f k xs
	insert k v (ListMap l) = ListMap $ f k v l
		where
			f k v [] = [(k, v)]
			f k v ((x, y):xs) | x == k 		= (k, v) : xs
												| otherwise = (x, y) : f k v xs

	delete k (ListMap l) = ListMap $ f k l
		where
			f k [] = []
			f k ((x, y):xs) | x == k 		= xs
											| otherwise = (x, y) : f k xs


x = ListMap [(1, ""), (2, "spam"), (42, "The Larch")]