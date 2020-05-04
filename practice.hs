data BinaryTree a =
     Empty
    | Node (BinaryTree a) a (BinaryTree a) 
    | Leaf a 
    deriving (Show, Eq)

