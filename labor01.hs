fakt1 :: Integer -> Integer
fakt1 0 = 1
fakt1 n = n * fakt1 (n-1)

fakt2 :: Int -> Int
fakt2 n
    | n < 0 = -1
    | n == 0 = 1
    | otherwise = n * fakt2 (n-1)

fakt3 :: Int -> Int -> Int
fakt3 res n
    | n < 0 = -1
    | n == 0 = res
    | otherwise = fakt3 (n * res) (n - 1)

fakt4 :: Int -> Int
fakt4 n = foldr (*) 1 [1..n]

fakt5 :: Int -> Int
fakt5 n = foldl (*) 1 [1..n]

fakt6 :: Int -> Int
fakt6 n = product [1..n]


