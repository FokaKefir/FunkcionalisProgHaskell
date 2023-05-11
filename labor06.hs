import Control.Exception
import System.IO
import System.IO (hPutStr)


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


fel1 = do
    putStr "n = "
    temp <- getLine
    let n = read temp :: Int
    print n

fel2 = do
    putStr "n1, n2 = "
    temp <- getLine
    let [tn1, tn2] = words temp
    let n1 = read tn1 :: Int
    let n2 = read tn2 :: Int
    print(n1, n2)

fel3 = do
    putStr "n1, n2 = "
    temp <- getLine
    let tls = words temp
    let [n1, n2] = map read tls :: [Int]
    print(n1, n2)

fel4 = do
    catch (do
        putStr "n1, n2 = "
        temp <- getLine
        let tls = words temp
        let [n1, n2] = map read tls :: [Int]
        print(n1, n2)
        ) fgcatch
            where
                fgcatch :: SomeException -> IO ()
                fgcatch e = do putStrLn $ "Error: " ++ show e 

primT :: (Integral a) => a -> a -> Bool
primT k nr
    | nr <= 1 = error "hibas bemenet"
    | nr == 2 = True
    | even nr = False
    | nr < k * k = True
    | mod nr k == 0 = False
    | otherwise = primT (k + 2) nr

primesBetweenInterval = catch (do
        putStr "n, m = "
        line <- getLine
        let [a, b] = map read (words line) :: [Int]
        let ls = [x | x <- [a..b], primT 3 x]
        mapM_ print ls
    ) fgcatch
        where
            fgcatch :: SomeException -> IO ()
            fgcatch e = do putStrLn $ "Error: " ++ show e 


--fibWriteFile = do
--    fout <- openFile "fibs.txt" WriteMode
--    mapM_ hPutStr $ fibFirstN 10