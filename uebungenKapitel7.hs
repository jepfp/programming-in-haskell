import Distribution.Simple.Utils (xargs)
import Prelude hiding (elem, (!!))

-- 1)
compExample :: (a -> b) -> (a -> Bool) -> [a] -> [b]
compExample x p = map x . filter p

-- 2)
-- a)
allOwn :: (a -> Bool) -> [Bool] -> Bool
allOwn p = foldr (&&) True

-- b)
anyOwn :: (a -> Bool) -> [Bool] -> Bool
anyOwn p = foldr (||) False

-- c)
takeWhileOwn :: (a -> Bool) -> [a] -> [a]
--takeWhileOwn p (x:xs) = if p x then x : takeWhileOwn p xs else []
takeWhileOwn p = foldr (\x acc -> if p x then x : acc else []) []

-- d)
dropWhileOwn :: (a -> Bool) -> [a] -> [a]
dropWhileOwn _ [] = []
dropWhileOwn p (x:xs) = if p x then dropWhileOwn p xs else x : xs

-- 3)
mapFAndFilterP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFAndFilterP f p = foldr (\x acc -> if p x then f x : acc else acc) []

-- 4)
dec2int :: [Int] -> Int
dec2int = foldl (\acc d -> acc * 10 + d) 0

-- 5)
curryOwn :: ((a, b) -> c) -> (a -> b -> c)
curryOwn f a b = f (a, b)


sumPair (a, b) = a + b

b = curryOwn sumPair

uncurryOwn :: (a -> b -> c) -> ((a, b) -> c)
--uncurryOwn f (a, b) = f a b
--better understandable
uncurryOwn f = \(x, y) -> f x y


-- 6)
--unfold from the book
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2BinUnfold = unfold (== 0) (`mod` 2) (`div` 2)

chop8Unfold = unfold null (take 8) (drop 8)

mapfUnfold :: (a -> b) -> [a] -> [b]
mapfUnfold f = unfold null (f . head) tail

iteratefUnfold f = unfold (const False) f (+1)

-- 7)
withParity xs = if odd (sum xs) then xs ++ [1] else xs ++ [0]

checkParity xs = if even (sum xs) then take 8 xs else error "parity bit error"

-- 8) skipped

-- 9)
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs


-- 10)

luhnDouble x
   | doubled > 9 = doubled - 9
   | otherwise = doubled
  where
    doubled = x * 2

luhnOld :: Int -> Int -> Int -> Int -> Bool
luhnOld d1 d2 d3 d4 =
  sum `mod` 10 == 0
  where
    doubledD1 = luhnDouble d1
    doubledD3 = luhnDouble d3
    sum = doubledD1 + d2 + doubledD3 + d4

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0


twoInOne = sum . altMap luhnDouble id 