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

