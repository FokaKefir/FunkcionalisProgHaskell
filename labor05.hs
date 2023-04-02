import Data.Char
import Data.List

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

osszegT :: [(String, Int)] -> Int
osszegT [] = 0
osszegT ((name, num) : ls) = num + osszegT ls

-- [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]), ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
atlagTu :: [(String, [Double])] -> IO()
atlagTu ls = mapM_ printData $  map (\(name, lsNum) -> (name, auxAtlag lsNum)) ls
    where 
        auxAtlag lsNum = (sum lsNum) / (genericLength lsNum)
        printData (name, avg) = do
            putStr (name ++ " ")
            print avg


myLength :: [a] -> Int
myLength ls = foldl (\ s _ -> s + 1) 0 ls

mySum :: Integral a => [a] -> a
mySum ls = foldl (+) 0 ls

myElem :: Eq a => a -> [a] -> Bool
myElem k ls = foldr (\ act cond -> (k == act || cond)) False ls

myReverse :: [a] -> [a]
myReverse ls = foldl (\ lsnew k -> (k : lsnew)) [] ls

myProduct :: [Int] -> Int
myProduct ls = foldl (*) 1 ls

myMaximum :: Ord a => [a] -> a
myMaximum (k:ls) = foldl max k ls

polinom :: [Int] -> Int -> Int
polinom ls x = foldr (\ a k -> a + x * k) 0 ls