ints = 1 : map succ ints

x = ['a'..'z'] !! 14 -- asks the 14th element of the list

data BTree a = Leaf a | Fork (BTree a) (BTree a) deriving (Show, Eq)

foo = Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)

bar = Fork (Fork (Fork (Fork (Leaf 5) (Leaf 4)) (Leaf 3)) (Leaf 2)) (Leaf 1)

zipWithBTrees :: (a -> b -> c) -> BTree a -> BTree b -> BTree c

zipWithBTrees f (Leaf x) (Leaf y) = Leaf (f x y)

zipWithBTrees f  (Fork xt xt') (Fork yt yt') = Fork (zipWithBTrees f xt yt) (zipWithBTrees f xt' yt')

data List a = Nil | Cons a (List a) deriving (Show, Eq)

ls0 = Cons 0 (Cons 1 (Cons 2 Nil))
ls1 = Cons 2 (Cons 1 (Cons 0 Nil))

zipWithLists :: (a -> b -> c) -> List a -> List b -> List c
zipWithLists _ Nil Nil = Nil
zipWithLists f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWithLists f xs ys)

class Zippable z where
    zipWith :: (a -> b -> c) -> z a -> z b -> z c
    