powerset' [] = [[]]
powerset' (x:xs) = half ++ map (x :) half where half = powerset' xs

