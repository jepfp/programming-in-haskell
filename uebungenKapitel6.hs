import Distribution.Simple.Utils (xargs)
import Prelude hiding (elem, (!!))

-- 1)
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

-- 2)
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown (x - 1)

-- 3)

myExp :: Int -> Int -> Int
myExp _ 0 = 1
myExp m n = m * myExp m (n - 1)

-- 4)
euclid :: Int -> Int -> Int
euclid x y
  | x < 0 || y < 0 = error "Both inputs to euclid must be non-negative integers"
  | x == y = x
  | x > y = euclid (x - y) y
  | otherwise = euclid y (y - x)

-- 5)
-- skipped

-- 6)
-- a)
ownAnd :: [Bool] -> Bool
ownAnd = foldr (&&) True

-- b)
ownConcat :: [[a]] -> [a]
ownConcat [] = []
ownConcat (x : xs) = x ++ ownConcat xs

-- suggestion Copilot
ownConcat2 :: [[a]] -> [a]
ownConcat2 = foldr (++) []

-- c)
ownReplicate :: Int -> a -> [a]
ownReplicate 0 _ = []
ownReplicate n x = x : ownReplicate (n - 1) x

-- d)
-- !! had been ignored from prelude in the imports at the beginning of the file
(!!) :: [a] -> Int -> a
[] !! _ = error "Cannot access element in an empty list"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

-- e)
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem a [x] = x == a
elem a (x : xs)
  | x == a = True
  | otherwise = elem a xs

-- with Copilot review
elem2 a = foldr (\x acc -> x == a || acc) False

-- 7)
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] a = a
merge (x:xs) (y:ys) = merge xs (if x < y then x : y : ys else y : merge [x] ys)

mergeCopilot :: Ord a => [a] -> [a] -> [a]
mergeCopilot a [] = a
mergeCopilot [] a = a
mergeCopilot (x:xs) (y:ys) = if x < y
                             then x : mergeCopilot xs (y : ys)
                             else y : mergeCopilot (x : xs) ys

-- 8)
halve :: [a] -> ([a],[a])
halve a = splitAt n a
        where n = length a `div`  2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
     where (ys, zs) = halve xs

-- 9)
-- a)
sumOfNumbers :: [Int] -> Int
sumOfNumbers = foldr ((+)) 0

-- b)
takeN :: Int -> [a] -> [a]
takeN 0 _ = []
takeN n (x:xs) = x : takeN (n-1) xs

-- c)
lastElement :: [a] -> a
lastElement (x:xs) | null xs = x
                   | otherwise = lastElement xs