import Data.Time.Format.ISO8601 (yearFormat)

-- 1)
data Nat = Zero | Succ Nat
  deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

-- with conversions (inefficient)
addWithConversion :: Nat -> Nat -> Nat
addWithConversion m n = int2nat (nat2int m + nat2int n)
multWithConversion :: Nat -> Nat -> Nat
multWithConversion m n = int2nat (nat2int m * nat2int n)

-- without conversions
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

-- 2)
data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf a) = x == a
occurs x (Node l y r) = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r

-- 3)
data OtherTree a = OtherLeaf a | OtherNode (OtherTree a) (OtherTree a)
     deriving Show

countLeaves :: OtherTree a -> Int
countLeaves (OtherLeaf _) = 1
countLeaves (OtherNode l r) = countLeaves l + countLeaves r

balanced :: OtherTree a -> Bool
balanced (OtherLeaf _) = True
balanced (OtherNode l r) = abs (countLeaves l - countLeaves r) <= 1 &&
                           balanced l &&
                           balanced r

-- 4)
splitIntoHalf :: [a] -> ([a], [a])
splitIntoHalf a = splitAt halfLength a
            where halfLength = length a `div` 2

balance :: [a] -> OtherTree a
balance [] = error "Cannot balance empty lists"
balance [a] = OtherLeaf a
balance a = OtherNode (balance l) (balance r)
     where (l, r) = splitIntoHalf a


-- 5)
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)

-- 6)
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 7) 
-- Note: These instances already exist in Prelude, so this is just for exercise
-- instance Eq a => Eq (Maybe a) where
--   Nothing == Nothing = True
--   Just x == Just y = x == y
--   _ == _ = False

-- instance Eq a => Eq [a] where
--   [] == [] = True
--   (x:xs) == (y:ys) = x == y && xs == ys
--   _ == _ = False

-- 8)
--skipped

-- 9)
-- in nächstem File