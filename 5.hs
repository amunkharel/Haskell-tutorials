data Tree a = Empty | Leaf a | Fork a (Tree a) (Tree a) deriving (Show, Eq)

--             j
--           /  \
--          f    k
--         / \  / \
--        a   h    z
--       / \
--          d
-- Depth First traversal- jfadhkz
-- Breadth First Traversal - jfkahzd
foo = Fork 'j' (Fork 'f' (Fork 'a' Empty (Leaf 'd')) (Leaf 'h')) (Fork 'k' Empty (Leaf 'z'))

dfs :: Tree a -> [a]
dfs Empty = []
dfs (Leaf x) = [x]
dfs (Fork x yt zt) =  x : (dfs yt ++ dfs zt) 

--LIFO
dfs' :: Tree a -> [a]
dfs' xt = loop [xt] []
  where loop [] acc1 = reverse acc1
        loop (Empty:acc0) acc1 = loop acc0 acc1
        loop ((Leaf x): acc0) acc1 = loop acc0 (x:acc1)
        loop ((Fork x yt zt): acc0) acc1 = loop (yt:zt:acc0) (x:acc1)

-- acc0            acc1
-- [j]             []
-- [f k]           [j]
-- [a h k]         [f j]
-- [d h k]         [a f j]
-- [h k]           [d a f j]
-- [k]             [h d a f j]
-- [z]             [k h d a f j]
-- []              [z k h d a f j]

-- jfkahzd
bfs :: Tree a -> [a]
bfs xt = loop [xt] []
    where loop [] acc1 = reverse acc1
          loop ((Empty):acc0) acc1 = loop acc0 acc1
          loop ((Leaf x):acc0) acc1 = loop acc0 (x:acc1)
          loop ((Fork x yt zt):acc0) acc1 = loop (acc0 ++ [yt] ++ [zt]) (x:acc1)

-- [j]             []
-- [f k]           [j]
-- [k a h]         [f j]
-- [a h z]         [k f j]
-- [h z d]         [a k f j]
-- [z d]           [h a k f j]
-- [d]             [z h a k f j]
-- []              [d z h a k f j]

-- Queue is amortized order 1
