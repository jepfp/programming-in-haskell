double x = x + x
quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div`length xs
  where
    a = 10
    xs = [1,2,3,4,5]

lastJep xs = xs !! (length xs - 1)

initVersion1 xs = take (length xs - 1) xs
initVersion2 xs = reverse (tail (reverse xs))