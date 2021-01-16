module K3_62309 where

import Data.List

-- Task1 

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

treeWords :: Tree Char -> [String]
treeWords EmptyTree = []
treeWords (Node value EmptyTree EmptyTree) = [[value]]
treeWords (Node value left EmptyTree) = (map ([value] ++) (treeWords left))
treeWords (Node value EmptyTree right) = (map ([value] ++) (treeWords right))
treeWords (Node value left right) = (map ([value] ++) (treeWords left)) ++ (map ([value] ++) (treeWords right))

-- Task 2
mapsTo :: Integral t => (t -> t) -> t -> t -> (t,t)
mapsTo f a b 
  | a > b = error "a must be less or equal than b"
  | otherwise = (minimum [f x | x <- [a..b]], maximum [f x | x <- [a..b]])