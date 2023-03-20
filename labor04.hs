
lista1 :: Integral t => Int -> [t]
lista1 n = take n [x * x | x <- [0, 2..]] 

lista1_ :: Integral t => Int -> [(t, t)]
lista1_ n = take n [(x, x * x) | x <- [0, 2..]]

-- print fuggveny rendre a lista ertekeit kiirja a mapM_ miatt
printLista1 :: Int -> IO ()
printLista1 n = mapM_ print $ take n [(x, x * x) | x <- [0, 2..]]

printLista1_ :: Int -> IO ()
printLista1_ n = mapM_ myPrint $ take n [(x, x * x) | x <- [0, 2..]] 
    where
        myPrint (t1, t2) = putStrLn $ show t1 ++ "^2 = " ++ show t2

lista2 :: Integral t => Int -> [t]
lista2 n = take n $ aux 1
    where
        aux k = [k | _ <- [1..k]] ++ aux (k + 1)


lista3 :: Integral t => Int -> [t]
lista3 n = take n $ aux 1
    where
        aux k = [2 * k | _ <- [1..k]] ++ aux (k + 1)

lista4 n = [n,n-1..1] ++ [1..n]

lista5 n = take n aux
    where
        aux = [True, False] ++ aux

lista6 :: Integral t => t -> [Int]
lista6 n = [aux x | x <- [0..n]]
    where
        aux x
            | mod x 3 == 0 = 0
            | mod x 3 == 1 = 1
            | mod x 3 == 2 = -1

lista6_ :: Integral t => Int -> [t]
lista6_ n = take n [aux x | x <- [0..]]
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


fibFirstN n = take n $ fibAux 0 1
    where 
        fibAux a b = a : fibAux b (a + b)

fibLessN n = takeWhile (<=n) $ fibAux 0 1
    where 
        fibAux a b = a : fibAux b (a + b)


fibBetween a b = dropWhile (<=a) $ takeWhile (<=b) $ fibAux 0 1
    where 
        fibAux a b = a : fibAux b (a + b)

fibBetween_ a b = takeWhile (<=b) $ dropWhile (<=a) $ fibAux 0 1
    where 
        fibAux a b = a : fibAux b (a + b)


-- [1, 20, 5, 3, 20, 12, 20, 1, 1] -> (20, [1, 4, 6])
maxPositions ls = (maxi, positions)
    where
        maxi = maximum ls
        positions = map snd $ filter (aux maxi) $ zip ls [0..]
            where
                aux num (k, pos) = (num == k)

maxPositions_ ls = (maxi, positions)
    where
        (maxi, positions) = aux ls (head ls) [0] 0
            where
                aux [] maxi positions ind = (maxi, positions)
                aux (k : ls) maxi positions ind
                    | k > maxi = aux ls k [ind] (ind + 1)
                    | k == maxi = aux ls maxi (positions ++ [ind]) (ind + 1)
                    | otherwise = aux ls maxi positions (ind + 1)
