-- 9)

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

-- Control Stack
type Cont = [Op]
data Op = EVALADDITION Expr | EVALMULTIPLICATION Expr | ADD Int | MULT Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVALADDITION y : c)
eval (Mult x y) c = eval x (EVALMULTIPLICATION y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADDITION y : c) n = eval y (ADD n : c)
exec (EVALMULTIPLICATION y : c) n = eval y (MULT n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval e []


-- value (Add (Add (Val 2) (Val 3)) (Val 4))
-- value (Mult (Mult (Val 2) (Val 3)) (Val 4))