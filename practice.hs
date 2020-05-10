data List a = Nil | Cons {car :: a, cdr :: (List a)} deriving (Show, Eq)


data Troika a b c = Troika a b c deriving (Show, Eq)

x = Cons 1 (Cons 2 Nil)
y = Cons True (Cons False Nil)
z = Cons 'a' (Cons 'b' Nil)

collect :: List a -> List b -> List c -> List (Troika a b c)

collect (Cons x Nil) (Cons y Nil) (Cons z Nil)= Cons (Troika x y z) Nil
collect (Cons x xt) (Cons y yt) (Cons z zt) = Cons (Troika x y z) (collect xt yt zt)

gather :: (a -> b -> b) -> b -> List a -> b
gather f g (Cons x Nil) = g
gather f g (Cons x xt) = f x (gather f g xt)

translate :: List a -> [a]

translate (Cons x Nil) = [x]
translate (Cons x xt) = x : (translate xt)

translate' = gather (\x y -> x : y)  []