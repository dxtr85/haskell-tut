-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

subTree = Node "uncle"
  (Node "aunt" Empty Empty)
  (Node "marzena"
    (Node "renata" Empty Empty)
    (Node "iwona" Empty Empty))

simpleTree = Node "parent"
  (Node "left child"  Empty Empty)
  (Node "right child" subTree Empty)

treeHeight Empty = 0
treeHeight (Node t a b) =
  1 + (max (treeHeight a) (treeHeight b))
