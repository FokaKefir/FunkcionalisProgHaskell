import Data.Char

mySplitAt :: Int -> [a] -> ([a], [a]) 
mySplitAt i ls = (bls, jls)
    where
        bls = take i ls
        jls = drop i ls

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem _ [] = True
myNotElem a (k:ls) 
    | k == a = False
    | otherwise = myNotElem a ls

myNotElem2 :: Eq a => a -> [a] -> Bool
myNotElem2 a ls = if null ls then True else 
    if head ls == a then False else myNotElem2 a $ tail ls

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (k:ve) = k ++ myConcat ve

-- madarNyelv "tavasz" => "tapavapasz"
-- madarNyelv "szamitogep" => "szapamipitopogepep"
madarNyelv :: [Char] -> [Char]
madarNyelv [] = []
madarNyelv (k:ls)
    | elem k "aeiouAEIOU" = [k, 'p', k] ++ madarNyelv ls
    | otherwise = k : madarNyelv ls

myRepeat :: a -> [a]
myRepeat k = k : myRepeat k

myReplicate :: Int -> a -> [a]
myReplicate i k = take i $ myRepeat k

myCycle :: [a] -> [a]
myCycle ls = ls ++ myCycle ls

myIterate :: (a -> a) -> a -> [a]
myIterate fg x = x : myIterate fg (fg x)

myAny :: (a -> Bool) -> [a] -> Bool
myAny fg [] = False
myAny fg (k:ls)
    | fg k = True
    | otherwise = myAny fg ls

myAll :: (a -> Bool) -> [a] -> Bool
myAll fg [] = True
myAll fg (k:ls)
    | not $ fg k = False
    | otherwise = myAll fg ls

--lsSt = ["function", "class", "Float", "higher-order", "monad", "tuple", "variable", "Maybe", "recursion"]
myMinList :: [String] -> [String]
myMinList ls = filter (\x -> length x == mini) ls
    where mini = minimum $ map length ls

myMinListWrite ls = mapM_ putStrLn $ myMinList ls

talalat :: Eq a => a -> [a] -> [Int]
talalat k ls = auxTalalat k ls 0 
    where
        auxTalalat :: Eq a => a -> [a] -> Int -> [Int]
        auxTalalat k [] ind = []
        auxTalalat k (e:ls) ind
            | k == e = ind : auxTalalat k ls (ind + 1)
            | otherwise = auxTalalat k ls (ind + 1)