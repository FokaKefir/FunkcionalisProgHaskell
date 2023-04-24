numsBetweenTwoNumber :: IO()
numsBetweenTwoNumber = do
    line <- getLine
    let a = read line :: Int
    line <- getLine
    let b = read line :: Int
    mapM_ print [a..b]

numsBetweenTwoNumber2 :: IO()
numsBetweenTwoNumber2 = do
    line <- getLine
    let [a, b] = map read (words line) :: [Int]
    mapM_ print [a..b]

numOfProperDivisors :: Int -> Int
numOfProperDivisors n = length $ [x | x <- [2..n-1], n `mod` x == 0]

lstNumsOfProperDivisors :: Int -> Int -> [(Int, Int)]
lstNumsOfProperDivisors a b = map (\x -> (x, numOfProperDivisors x)) [a..b]

maximumOfProperDivisorsBetweenNums :: IO()
maximumOfProperDivisorsBetweenNums = do
    line <- getLine
    let [a, b] = map read (words line) :: [Int]
    let lst = lstNumsOfProperDivisors a b
    let maxi = foldl (\ x (_, y) -> max x y) 0 lst
    let flst = filter (\(a, b) -> b == maxi) lst
    putStrLn $ "Max proper divisor: " ++ show maxi
    mapM_ (\(a, b) -> print a) flst

fibFirstN n = take n $ fibAux 0 1
    where 
        fibAux a b = a : fibAux b (a + b)

printFirstNFibNumber :: IO()
printFirstNFibNumber = do
    line <- getLine
    let n = read line :: Int
    let fibLst = fibFirstN n
    mapM_ (\x -> putStr (show x ++ " ")) fibLst
