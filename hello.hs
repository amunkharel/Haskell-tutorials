
import System.IO

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

end a = head (reverse a)

positions :: Eq a => a -> [a] -> [Int]

positions x xs = 
    [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs - 1

