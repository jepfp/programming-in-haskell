import Data.Char

-- 1)

sumOfFirstHundretIntegerSuares :: Int
sumOfFirstHundretIntegerSuares = sum (map (\x -> x * x) [1..100])

-- 2)
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3)
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4)
myReplicate :: Int -> a -> [a]
myReplicate n a = map (const a) [0..n]

-- 5)
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6)
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- 7)
nestedGenerators = concat [[(x,y) | x <- [1, 2]] | y <- [3, 4]]
-- [(1,3),(2,3),(1,4),(2,4)]

-- 8)
-- our own find method as introduced in the book (does not just return the first element!)

find k t = [v | (k', v) <- t, k == k']

positions a xs = find a zipped
    where zipped = zip xs [1..]

-- 9)
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct a b = sum [a !! index * b !! index | index <- [0 .. length a - 1]]

-- optimized with Copilot:
scalarproductOptimized  :: [Int] -> [Int] -> Int
scalarproductOptimized  a b = sum (zipWith (*) a b)

-- 10)
let2int c = ord c - ord 'a'
int2let n = chr (ord 'a' + n)

shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = toUpper (shift n (toLower c))
          | otherwise = c
encode n xs = [shift n x | x <- xs]

