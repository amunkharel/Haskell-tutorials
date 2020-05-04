data List a = Nil | Cons {car :: a, cdr:: List a} deriving (Show, Eq)

bar = Cons 'a' (Cons 'b' (Cons 'c' Nil))

append xs ys = if xs == Nil
               then ys
               else Cons (car xs ) (append (cdr xs) ys)

append Nil ys = ys
append (Cons x xs) ys= Cons x (append xs ys)

reverse' Nil = Nil 
reverse' (Cons x xs) = append (reverse' xs) (Cons x Nil)

haskell2scheme :: [a] -> List a
haskell2scheme [] = Nil
haskell2scheme (x:xs) = Cons x (haskell2scheme xs)

scheme2haskell :: List a -> [a]
scheme2haskell Nil = []
scheme2haskell (Cons x xs) = x : scheme2haskell xs

data BTree a = Leaf a | Fork ( BTree a) (BTree a) deriving (Show, Eq)

tree = Fork (Fork (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c')) (Leaf 'd')) (Leaf 'e')

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