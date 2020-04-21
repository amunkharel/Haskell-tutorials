compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x == y
                    then compress (y:xs)
                    else x:(compress (y:xs))

{-
zip3 :: [x] -> [y] -> [z] -> [(x,y,z)]
zip3 [] _ _ = []
zip3 _ [] _ = []
zip3 _ _ [] = []
zip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3 xs ys zs





fst3' :: (x,y,z) -> x
fst3' (x,_,_) = x

snd3' :: (x,y,z) -> y
snd3' (_,y,_) = y

thrd3' :: (x,y,z) -> z
thrd3' (_,_,z) = z

unzip3 :: [(a,b,c)] -> ([a], [b], [c])
unzip3 [] = ([], [], [])
unzip3 ((a,b,c):xs) = (a:(fst3' rest), b:(snd3' rest), c:(thrd3' rest))
    where rest = unzip3 xs

-}


nub :: Eq a => [a] -> [a]
nub [ ] = [ ]
nub (x:xs) = x : nub (filter (/= x) xs)

