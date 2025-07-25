-- main program
-- compile with ghc -O2 countdown.hs
-- run with ./countdown
main :: IO ()
main = print (solutions' [1, 3, 7, 10, 25, 50] 765)

-- arithmetic operations
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- without performance optimization
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- With "Exploiting algebraic properties"
-- ⚠️ using this version will not return True for many valid statements...
-- valid :: Op -> Int -> Int -> Bool
-- valid Add x y = x <= y
-- valid Sub x y = x > y
-- valid Mul x y = x /= 1 && y /= 1 && x <= y
-- valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- numberic expressions
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
  [ apply o x y | x <- eval l, y <- eval r, valid o x y]

-- combinatorial functions
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
-- from book
-- choices = concat . map perms . subs

-- after exercise 1)
choices ns = [choice | sub <- subs ns, choice <- perms sub]

-- formalising the problem
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

-- testing solution with (25-10) * (50+1) = 15 * 51 = 765
-- e :: Expr
-- e = App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1))
-- solution e [1,3,7,10,25,50] 765

-- Brute force solution
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r
  ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- Combining generation and evaluation

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry
  ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e, m) <- results ns', m == n]

-- Exercises
-- 2)
removeOne :: (Eq a) => a -> [a] -> [a]
removeOne x (y : ys)
  | x == y = ys
  | otherwise = y : removeOne x ys

isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) ys = elem x ys && isChoice xs (removeOne x ys)

-- 3)
-- stack overflow

-- 4)