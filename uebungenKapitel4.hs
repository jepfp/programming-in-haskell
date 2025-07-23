-- how to load for running: ghci uebungenKapitel4.hs
-- 1)

halve :: [Int] -> ([Int], [Int])
halve xs = splitAt (length xs `div` 2) xs

-- 2)
third :: [a] -> a
-- third xs = head (tail (tail xs))
-- third xs = xs !! 2
third (_ : _ : z : _) = z

-- 3)
safetail :: [a] -> [a]
-- safetail a = if null a then [] else tail a

-- safetail a
--   | null a = []
--   | otherwise = tail a

safetail [] = []
safetail xs = tail xs

-- 4)
(|||) :: Bool -> Bool -> Bool
-- (|||) True True = True
-- (|||) False True = True
-- (|||) True False = True
-- (|||) False False = False

-- (|||) True _ = True
-- (|||) _ True = True
-- (|||) _ _ = False

-- (|||) False False = False
-- (|||) _ _ = True

a ||| b
  | a = True
  | b = True
  | otherwise = False

-- 5)
-- (&&&) :: Bool -> Bool -> Bool
-- a &&& b = if a then (if b then True else False) else False

-- 6)
a &&& b = if a then b else False

multiplyTwoNumbers x = \y -> x * y

-- 7)
mult :: Int -> Int -> Int -> Int
mult x = \y -> \z -> x * y * z

-- 8)
luhnDouble :: Int -> Int
luhnDouble x = if doubled <= 9 then doubled else doubled - 9
  where
    doubled = x * 2

luhnDouble2 x
   | doubled > 9 = doubled - 9
   | otherwise = doubled
  where
    doubled = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn d1 d2 d3 d4 =
  sum `mod` 10 == 0
  where
    doubledD1 = luhnDouble d1
    doubledD3 = luhnDouble d3
    sum = doubledD1 + d2 + doubledD3 + d4