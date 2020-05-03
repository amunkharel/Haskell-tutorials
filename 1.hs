map' f [] = []

map' f(x:xs) = f x : map' f xs

addToList = map' (+ 1) 

filter' f (x:xs) = if f x then x:rest else rest
                where rest = filter' f xs

intersect [] _ = []
intersect xs ys = [x' | x' <- xs, x' `elem` ys]

increasing [] = True
increasing [x] = True
increasing (x:xs) = if x < (head xs)
                    then increasing (xs)
                    else False

decimate xs = [x'| x' <- xs, x' `mod` 10 /= 0]

enchiper _ _ [] = []
enchiper xs ys (z:zs) = if f == []
                      then z:enchiper xs ys zs
                      else snd (head f):(enchiper xs ys zs)
    where t = zip xs ys
          f = filter ((== z) . fst) t
