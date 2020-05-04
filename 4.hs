data BTree a = Leaf a | Fork ( BTree a) (BTree a) deriving (Show, Eq)

tree = Fork (Fork (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c')) (Leaf 'd')) (Leaf 'e')

tree1 = Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)

size :: BTree a -> Int

size (Leaf _) = 1
size (Fork xt yt) = size xt + size yt

height :: BTree a -> Int

height (Leaf _) = 0
height (Fork xt yt) = 1 + max (height xt) (height yt)

flatten :: BTree a -> [a]

flatten (Leaf x) = [x]

flatten (Fork xt yt) = flatten xt ++ flatten yt

makeBTree :: [a] -> BTree a

makeBTree [x] = Leaf x

makeBTree xs = Fork (makeBTree (take m xs)) (makeBTree (drop m xs))
    where m = length xs `div` 2

mapBTree :: (a -> b) -> BTree a -> BTree b
mapBTree f (Leaf x) = Leaf (f x)
mapBTree f (Fork xt yt) = mapBTree f xt `Fork` mapBTree f yt


foldBTree :: (a -> b) -> (b -> b -> b) -> BTree a -> b
foldBTree f g (Leaf x) = f x
foldBTree f g(Fork xt yt) = g (foldBTree f g xt) (foldBTree f g yt)

size' xt = foldBTree (const 1) (+) xt

height' = foldBTree (const 0) (\ x y -> 1 + x `max` y)

flatten' = foldBTree (\x -> [x]) (++)

mapBTree' f = foldBTree (\x-> Leaf(f x)) Fork

-- Rose tree can have any number of children 

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)
x = Node 5 [Node 1 [], Node 2 [Node 3 [], Node 4 [Node 5 []]]]

fringe (Node x []) = [x]
fringe (Node _ xs) = foldr (++) [] (map fringe xs)

flattenRoseTree :: RoseTree a -> [a]
flattenRoseTree (Node x []) = [x]
flattenRoseTree (Node x xs) = x : concatMap flattenRoseTree xs

sumRoseTree :: Num a => RoseTree a -> a
sumRoseTree (Node x []) = x
sumRoseTree (Node x xs) = x + (sum $ map sumRoseTree xs)