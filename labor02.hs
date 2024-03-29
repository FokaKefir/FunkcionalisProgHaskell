-- HF 2. labor I

szamjegyekSzorzata :: Integral t => t -> t
szamjegyekSzorzata 0 = 1
szamjegyekSzorzata n = (n `mod` 10) * szamjegyekSzorzata (n `div` 10)

szamjegyekOsszege :: Integral t => t-> t
szamjegyekOsszege 0 = 0
szamjegyekOsszege n = (n `mod` 10) + szamjegyekOsszege (n `div` 10)

szamjegyekSzama :: Integral t => t -> t
szamjegyekSzama n
    | n < 10 = 1
    | otherwise = 1 + szamjegyekSzama (n `div` 10)

szamjegyekOsszegeK :: Integral t => t -> t -> t
szamjegyekOsszegeK n k
    | n == 0 = 0
    | (n `mod` 10) == k = k + szamjegyekOsszegeK (n `div` 10) k
    | otherwise = szamjegyekOsszegeK (n `div` 10) k

parosSzamjegyek :: Integral t => t -> t
parosSzamjegyek 0 = 0
parosSzamjegyek n
    | even n = 1 + parosSzamjegyek (n `div` 10)
    | otherwise = parosSzamjegyek (n `div` 10)

legnagyobbSzamjegy :: Integral t => t -> t
legnagyobbSzamjegy n
    | n < 10 = n
    | otherwise = max (n `mod` 10) (legnagyobbSzamjegy (n `div` 10))


szamjegyekSzamaBD :: Integral t => t -> t -> t -> t
szamjegyekSzamaBD n b d
    | n == 0 = 0
    | (n `mod` b) == d = 1 + szamjegyekSzamaBD (n `div` b) b d
    | otherwise = szamjegyekSzamaBD (n `div` b) b d

fib :: Integral t => t -> t 
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
    

fib2 :: Integral t => t -> t 
fib2 0 = 0
fib2 1 = 1
fib2 n = auxfib n 0 1
    where 
        auxfib n a b
            | n == 1 = b
            | otherwise = auxfib (n - 1) b (a + b)


-- II. resz
fibList :: Integral t => t -> [t]
fibList 0 = [0]
fibList 1 = [0, 1]
fibList n = xs ++ [k]
    where
        xs = fibList (n - 1)
        k = last xs + last (init xs)

fibList2 :: Integral t => t -> [t]
fibList2 n = 0 : auxFibList n 0 1 
    where
        auxFibList 0 a b = []
        auxFibList n a b = b : auxFibList (n - 1) b (a + b)

fibgo n (a, b)
    | n == 0 = a
    | otherwise = fibgo (n - 1) (b, a + b)

fibListInf = 0 : 1 : auxFibList fibListInf
    where
        auxFibList (a : (b : ve)) = (a + b) : auxFibList (b : ve)

fibList3 n = take n fibListInf

fibList4 n = takeWhile (<n) fibListInf




