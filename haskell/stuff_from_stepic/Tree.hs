module Tree where

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) 	= 0
height (Node x y) = max (height x) (height y) + 1

size :: Tree a -> Int
size (Leaf _) 	= 1
size (Node x y) = 1 + size x + size y

printTree :: Show a => Tree a -> String
printTree (Leaf a) 		= show a
printTree (Node a b)  = printTree a ++ printTree b