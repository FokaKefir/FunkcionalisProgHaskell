myLength :: [a] -> Int
myLength [] = 0
myLength (_ : ls) = 1 + myLength ls

myLength3 :: [a] -> Int
myLength3 ls = auxLength ls 0
    where
        auxLength [] count = count
        auxLength (_ : ve) count = auxLength ve (count + 1)

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (k : ve) = k * myProduct ve

myProduct2 :: Num a => [a] -> a
myProduct2 [] = error "Empty list"
myProduct2 [k] = k
myProduct2 (k : ve) = k * myProduct ve

myMaximum :: Ord t => [t] -> t
myMaximum [] = error "Empty list"
myMaximum [k] = k
myMaximum (k : vs)
    | k > maxi = k
    | otherwise = maxi
    where 
        maxi = myMaximum vs 

myListPos :: [a] -> Int -> a 
myListPos ls k
    | (length ls) <= k = error "index error"
    | k == 0 = head ls
    | otherwise = myListPos (tail ls) (k - 1)

myAppendLists :: [a] -> [a] -> [a]
myAppendLists [] ls = ls
myAppendLists ls1 ls2
    | null ls1 = ls2
    | otherwise = myAppendLists ls (k : ls2)
    where
        k = last ls1
        ls = init ls1

myPalindrome :: Eq a => [a] -> Bool
myPalindrome [] = True
myPalindrome [k] = True
myPalindrome (f : ve)
    | f /= l = False
    |otherwise = myPalindrome (init ve)
    where 
        l = last ve
        
numberOfDigits :: (Integral t) => t -> t
numberOfDigits 0 = 0
numberOfDigits k = 1 + numberOfDigits (k `div` 10)
    
putBack :: [a] -> [a]
putBack [] = []
putBack (k : ve) = ve ++ [k]


average :: Fractional a => [a] -> a
average ls = auxavg ls 0 0
    where 
        auxavg :: (Fractional a) => [a] -> a -> a -> a
        auxavg [] s n = s / n
        auxavg (k : ve) s n = auxavg ve (s + k) (n + 1)


decToBase :: Integral t => t -> t -> [t]
decToBase 0 b = []
decToBase n b = (decToBase (n `div` b) b) ++ [n `mod` b]

baseToDec :: Integral t => [t] -> t -> t
baseToDec ls b = auxBTD ls b (b ^ (length ls - 1))
    where
        auxBTD :: Integral t => [t] -> t -> t -> t
        auxBTD [] b bpow = 0
        auxBTD (k : ls) b bpow = (k * bpow) + auxBTD ls b (bpow `div` b)
