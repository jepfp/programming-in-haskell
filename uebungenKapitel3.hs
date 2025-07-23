-- apply :: (a -> b) -> a -> b
apply f x = f x


-- 3
swap (x,y)=(y,x)

pair x y = (x,y)

double x = x*2

palindrome xs = xs == reverse xs
twice f x = f (f x)

