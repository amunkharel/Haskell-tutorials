-- parametric polymorphism, example: head :: [a] -> a

-- Second type- ad hoc polymorphism, example show :: Show a => a -> String

-- 300 BC Euclidian algorithm for finish Greatest Common divisor

-- 8/4 => 2/1 
-- 3/9 => 1/3 

gcd' (x,y) = 
    if x == y then x
    else if x < y then gcd' (x, y - x)
    else gcd' (x - y , y)

lcm' (x,y) = head [x*i | i <- [1..y], j <- [1..x], x *i == y *j]

data Rat = Rat {numr :: Int, denr :: Int}

instance Show Rat where
    show (Rat x 1) = show x -- this is not recursive
    show (Rat 0 _) = "0"
    show (Rat x y) = show x ++ "/" ++ show y


(%) :: Int -> Int -> Rat
x % y = let z = gcd x y in Rat (x `div` z) (y `div` z)


-- When we do Rat 1 2 == Rat 2 4, it will give us false with our Eq instance
-- a/b = c/d a * d = c * b

instance Eq Rat where
    (Rat a b) == (Rat c d) = a * d == b * c

instance Num Rat where
    (Rat a b) * (Rat c d) = (a * c) % (b * d)
    (Rat a b) + (Rat c d) = (a * d + c * b) % (b * d)
    fromInteger x = fromIntegral x % 1

instance Enum Rat where
    succ (Rat a b) = if a < b 
                     then succ a % b 
                     else 1 % succ b