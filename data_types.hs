data Tree = Empty
          | Leaf Int
          | Node Tree Tree

depth :: Tree -> Int
depth Empty = 0
depth (Leaf n) = 1
depth (Node l r) = 1 + max (depth l) (depth r)

--data BinTree a = Leaf a | Node (BinTree a) a (BinTree a)
--     deriving (Eq, Show)