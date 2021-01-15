module Task1 where

data Tree a = EmptyTree | Node {
                            value :: a,
                            left :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)

values :: Strategy -> (Tree a) -> [a]
values _ EmptyTree = []
values Inorder t = (values Inorder (left t)) ++ [value t] ++ (values Inorder (right t))
values Postorder t = (values Postorder (left t)) ++ (values Postorder (right t)) ++ [value t]
values Preorder t = [value t] ++ (values Preorder (left t)) ++ (values Preorder (right t))


