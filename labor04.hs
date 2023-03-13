lista5 :: Integral t => t -> [Int]
lista5 n = [aux x | x <- [0..n]]
    where
        aux x
            | mod x 3 == 0 = 0
            | mod x 3 == 1 = 1
            | mod x 3 == 2 = -1

lista5_ :: Integral t => Int -> [t]
lista5_ n = take n [aux x | x <- [0..]]
    where
        aux x
            | mod x 3 == 0 = 0
            | mod x 3 == 1 = 1
            | mod x 3 == 2 = -1


osztokSzama :: Integral a => a -> Int
osztokSzama n = length [x | x <- [1..n], mod n x == 0]

osztok :: Integral p => p -> ([p], Int)
osztok n = (osztok, osztokSz)
    where
        osztok = [x | x <- [1..n], mod n x == 0]
        osztokSz = length osztok

osztokSzamaRec :: Integral a => a -> Int
osztokSzamaRec n = osztokSzamaRecAux n 1
    where
        osztokSzamaRecAux n x
            | x > div n 2 = 1
            | mod n x == 0 = 1 + osztokSzamaRecAux n (x + 1)
            | otherwise = osztokSzamaRecAux n (x + 1)

osztokRec :: Integral t => t -> (t, [t])
osztokRec n = osztokRecAux n 1
    where
        osztokRecAux n x
            | x > div n 2 = (1, [n])
            | mod n x == 0 = (s + 1, x : ls)
            | otherwise = (s, ls)
                where
                    (s, ls) = osztokRecAux n (x + 1)

osztokRec_ :: Integral t => t -> (t, [t])
osztokRec_ n = osztokRecAux n 1 (0, [])
    where
        osztokRecAux n x (osz, oszList)
            | x > div n 2 = (osz + 1, n : oszList)
            | mod n x == 0 = osztokRecAux n (x + 1) (osz + 1, x : oszList)
            | otherwise = osztokRecAux n (x + 1) (osz, oszList)

myReverse :: [t] -> [t]
myReverse [] = []
myReverse (k : ls) = myReverse ls ++ [k]

--nthList :: [t] -> Int -> [t]
nthList ls n = auxNthList ls n 1
    where
        auxNthList ls n k
            | null ls = []
            | k == n = head ls : auxNthList (tail ls) n 1
            | otherwise = auxNthList (tail ls) n (k + 1)