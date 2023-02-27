import Data.List
import Data.Char

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


osszeg :: Int -> Int -> Int
osszeg a b = a + b

kulonbseg :: Int -> Int -> Int
kulonbseg a b = a - b

--osztas :: Double -> Double -> Double
osztas :: Fractional a => a -> a -> a
--osztas a b = a / b
osztas a b = (/) a b

osztasiEgesz :: Integral a => a -> a -> a
--osztasiEgesz a b = a `div` b
osztasiEgesz a b = div a b

osztasiMaradek :: Integral a => a -> a -> a
osztasiMaradek a b = a `mod` b

elsofokuGyok :: (Fractional a, Eq a) => a -> a -> a
elsofokuGyok a b 
    | a /= 0 = (-b) / a
    | otherwise = error "0-val osztas nem lehetseges" 

abszolut :: (Num a, Ord a) => a -> a
abszolut a 
    | a < 0 = -a
    | otherwise = a

maxi :: Ord a => a -> a -> a
maxi a b 
    | a > b = a 
    | otherwise = b

masodfokuGyok :: Double -> Double -> Double -> (Double, Double)
masodfokuGyok a b c 
    | a == 0 = error "0-val nem lehet osztani"
    | delta < 0 = error "komplex gyokok"
    | otherwise = (x1, x2)
    where 
        delta = b * b - 4 * a * c
        x1 = (-b + sqrt delta) / (2 * a)
        x2 = (-b - sqrt delta) / (2 * a)

elempar :: Eq a => (a, a) -> (a, a) -> Bool
elempar a b
    | a1 == b1 && a2 == b2 = True
    | a1 == b2 && a2 == b1 = True
    | otherwise = False
    where
        (a1, a2) = a
        (b1, b2) = b

hatvany :: (Integral a, Floating b) => b -> a -> b
hatvany x n
    | n == 0 = 1
    | otherwise = x * hatvany x (n - 1)

hatvany1 :: (Integral a, Floating b) => b -> a -> b
hatvany1 x n = if n == 0 then 1 else x * hatvany1 x (n - 1)

hatvany2 :: (Integral a, Floating b) => b -> a -> b
hatvany2 x n = auxhatvany 1 x n
    where
        auxhatvany res x n
            | n == 0 = res
            | otherwise = auxhatvany (res * x) x (n - 1)


-- halmazok
fgv1 :: (Floating a, Enum a) => a -> [a]
fgv1 n = map sqrt [1..n]

fgv1_ :: (Floating a, Enum a) => a -> [(a, a)]
fgv1_ n = map mysqrt [1..n]
    where
        mysqrt i = (i, sqrt i)

fgv2 :: Integral a => a -> [a]
fgv2 n = map (\x -> x * x) [1..n]

fgv2_ :: Integral a => a -> [(a, a)]
fgv2_ n = map (\x -> (x, x * x)) [1..n]

fgv3 :: Integral a => a -> [a]
fgv3 n = map myCube [1..n]
    where
        myCube i = i * i * i

fgv4 :: Integral a => a -> [a]
fgv4 n = [1..n] \\ (fgv2 n)

fgv4_ :: Integral a => a -> [a]
fgv4_ n = filter myTest [1..n]
    where
        myTest i = t2 * t2 /= i
            where
                t1 = sqrt (fromIntegral i)
                t2 = round t1

fgv5 :: Integral a => a -> a -> [a]
fgv5 x n = map (^ x) [1..n]
--fgv5 x n = map (x ^) [1..n]

fgv6 :: Integral a => a -> [a]
fgv6 n = filter (myTestDiv n) [2,4..div n 2]
    where 
        myTestDiv :: Integral a => a -> a -> Bool
        myTestDiv n i = mod n i == 0

fgv7 :: Integral a => a -> [a]
fgv7 n = 2 : filter (myTestPrime 3) [3, 5..n]
    where
        myTestPrime i x
            | i * i  > x = True
            | x `mod` i == 0 = False
            | otherwise = myTestPrime (i + 2) x

primes = filterPrime [3, 5..] 
    where filterPrime (p : xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

primesL n = 2 : takeWhile (<= n) primes 

fgv8 :: Integral a => a -> [a]
fgv8 n = filter (myTestComp 2) [2..n]
    where
        myTestComp i x
            | i * i > x = False
            | mod x i == 0 = True
            | otherwise = myTestComp (i + 1) x

fgv8_ :: Integral a => a -> [a]
fgv8_ n = [2..n] \\ (fgv7 n)

fgv9 :: Integral a => a -> [(a, a, a)]
fgv9 n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

fgv10 :: [(Char, Int)]
fgv10 = [(x, ord x - ord 'a') | x <- ['a'..'z']]




main :: IO ()
main = do
    let ossz = osszeg 1 (-4)
    putStr "osszeg: "
    print ossz

    let oszt = osztas 4 2
    putStr "osztas: "
    print oszt

    let oe = osztasiEgesz 5 2
    putStr "osztasi egesz: "
    print oe

    let om = osztasiMaradek 15 8
    putStr "osztasi maradek: "
    print om
