{-
Projekt link: http://lambda.inf.elte.hu/Graphs.xml
-}

import Data.List

graph1 :: [(Int,Int)]
graph1 = []
graph2 = [(1,2), (2,4), (2,3), (4,1)]
graph3 = [(1,2), (2,3), (3,4), (4,1)]
graph4 = [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]
graph5 = [(3,4), (6,8), (5,3), (5,4), (2,1), (6,7), (8,7)]
graph6 = [(2,3)]

{- Csomopontok listaja egy grafban!
Az unzip az egy tuple tipusu listabol keszit ket kulonbozo listat
Ezek kikerulnek az ls1-be es ls2-be
A nub fuggveny pedig a dupla elemeket veszi ki az osszefuzott listabol
-}
nodesOf :: Eq a => [(a, a)] -> [a]
nodesOf ls = nub $ ls1 ++ ls2
    where
        (ls1, ls2) = unzip ls

{- Csomopontok szama egy grafban!
Lekerjuk a hosszat a csucsok listanak
-}
numberOfNodes :: Eq a => [(a, a)] -> Int
numberOfNodes ls = length $ nodesOf ls

{- Vizsgalja hogy ket csomopont szomszedos-e a grafban!
Az elem segitsegevel hogy a csomopont par benne van-e a grafban
-}
areNeighbors :: Eq a => [(a, a)] -> a -> a -> Bool
areNeighbors graph x y = (x, y) `elem` graph || (y, x) `elem` graph

{- Teljes-e a graf?
Egy graf akkor teljes ha az elek szama egyenlo n*(n-1)/2 ahol n jeloli a csucsok szamat
-}
isCompleteGraph :: Eq a => [(a, a)] -> Bool
isCompleteGraph [] = False
isCompleteGraph graph = length graph == n * (n - 1) `div` 2
    where
        n = numberOfNodes graph

{- Egy adott csomopont szomszedai egy grafban!
A filter segitsegevel szurjuk azokat az eleket amikben szerepel a megadott csucspont
A map segitsegevel kivalasztjuk a tuple-bol az adott szomszedot
-}
neighborsOf :: Eq a => [(a, a)] -> a -> [a]
neighborsOf graph k = map (\(x, y) -> if x == k then y else x) $ filter (\(x, y) -> x == k || y == k) graph

{- Regularis-e a graf? Csak akkor regularis egy graf, ha mindegyik csomopontbol ugyan annyi el indul
Eloszor lekerjuk a csomopontokat (nodesOf gr)
Majd a map es lambda fuggveny segitsegevel meghatarozzuk, hogy mindegyik csomopontra hany el jut
A nub fuggveny ugyebar szuri a duplikatumokat, tehat, 
ha mindegyik csomoponthoz k db el van kapcsolva akkor az egy egyelemu listat fog visszateriteni
-}
isRegularGraph :: Eq a => [(a, a)] -> Bool
isRegularGraph [] = False
isRegularGraph gr = 1 == (length $ nub $ map (\x -> length $ neighborsOf gr x) $ nodesOf gr)
    
{- Megadja az utakat ket csomopont kozott!
Egy seged listat fogunk hasznalni amiben taroljuk az utat amit mar aktualisan megtettunk
Kezdetben a cel fog belekerulni es onnan keressuk x-hez az utat
node - jeloli mindig az aktualis csomopontot a bejarasban
neighborsOfNode - ennek a szomszediat
possibleNeighborsOfNode - azokat a szomszedokat ahol meg nem jartunk a bejaras soran
Ha ezek kozott megtalalhato a nodeRes az azt jelenti hogy megvan egy ut
solutions - pedig jeloli a tobbi utat, amit ugy erunk el hogy a rekurziot meghivjuk soronkent a szomszedokra 
            es mindig az aktualis szomszedot belehelyezzuk az addig megtett utba;
Ez egy olyan tombot terit vissza ami mindegyik szomszed iranyaban visszateriti a megoldasokat, ezert kell hasznalni a concat-et, 
    hogy a reszmegoldasokat egybefuzze
-}
pathsBetween :: Eq a => [(a, a)] -> a -> a -> [[a]]
pathsBetween gr x y = pathTo gr x [y]
    where
        pathTo :: Eq a => [(a, a)] -> a -> [a] -> [[a]]
        pathTo gr nodeRes path = if nodeRes `elem` possibleNeighborsOfNode
                            then [nodeRes : path] ++ concat solutions
                            else concat solutions
            where
                node = head path
                neighborsOfNode = neighborsOf gr node
                possibleNeighborsOfNode = filter (\n -> n `notElem` path) neighborsOfNode
                solutions = map (\n -> pathTo gr nodeRes (n:path)) possibleNeighborsOfNode


{- Figyeli hogy ket csomopont ossze van-e kotve
Az elozo algoritmushoz hasonloan mukodik
csupan a filter resz azokat a csomopontokat fogja szurni amelyekbol tovabb lehet haladni a megoldas csomoponthoz
Ha tobb mint egy ilyen van akkor el lehet jutni egyikbol a masikba
-}
areConnected :: Eq a => [(a, a)] -> a -> a -> Bool
areConnected gr x y = auxAreConnected gr x [y]
    where
        auxAreConnected :: Eq a => [(a, a)] -> a -> [a] -> Bool
        auxAreConnected gr nodeRes path = if nodeRes `elem` possibleNeighborsOfNode 
                            then True 
                            else 0 < (length $ filter (\n -> auxAreConnected gr nodeRes (n : path)) possibleNeighborsOfNode)
            where
                node = head path
                neighborsOfNode = neighborsOf gr node
                possibleNeighborsOfNode = filter (\n -> n `notElem` path) neighborsOfNode

{- Osszefuggo-e egy graf? 
Akkor osszefuggo ha az osszes csomopontbol el lehet jutni az osszes tobbi csomopontba
Elsosorban vizsgaljuk hogy egy adott csomopontbol el lehet-e jutni az osszes tobbibe
Ezt az elozoleg implementalt areConnected fuggveny es filter segitsegevel erjuk el
amely szuri azokat a csomopontokat amelyek elerhetok a kezdocsomopont szamara
Ha egy csomopontbol el lehet jutni az osszes tobbibe, 
akkor egy iranyitatlan grafra igaz az hogy az osszes tobbibol is barhova el lehet erni
-}
isConnectedGraph :: Eq a => [(a, a)] -> Bool
isConnectedGraph [] = False
isConnectedGraph gr = length nodes == length conns
    where
        nodes = nodesOf gr
        conns = nub $ connections gr (head nodes) []
        connections :: Eq a => [(a, a)] -> a -> [a] -> [a]
        connections gr node lsNodes
            | node `elem` lsNodes = conns
            | otherwise = node : conns
                where 
                    neighborsOfNode = neighborsOf gr node
                    possibleNeighborsOfNode = filter (\n -> n `notElem` lsNodes) neighborsOfNode
                    conns = concat $ map (\n -> connections gr n (node : lsNodes)) possibleNeighborsOfNode

{-
isConnectedGraph :: Eq a => [(a, a)] -> Bool
isConnectedGraph [] = False
isConnectedGraph gr = (length nodes) == (length $ filter (\n -> areConnected gr fnode n) nodes)
    where
        (fnode : nodes) = nodesOf gr
-}



main = do
    putStrLn "Graph2 nodes: "
    print $ nodesOf graph2

    putStrLn "Number of nodes graph2: "
    print $ numberOfNodes graph2

    putStrLn "Are 1 and 4 neighbors in graph3?"
    print $ areNeighbors graph2 1 4

    putStrLn "Is graph4 completed?"
    print $ isCompleteGraph graph4

    putStrLn "Neighbors of 8 in graph5: "
    print $ sort $ neighborsOf graph5 8

    putStrLn "Is graph3 a regular graph?"
    print $ isRegularGraph graph3

    putStrLn "Are 2 4 connected in graph4?"
    print $ areConnected graph4 2 4 

    putStrLn "Paths between 2 and 4 in graph3:"
    print $ sort $ pathsBetween graph4 2 4

    putStrLn "Is graph4 connected graph?"
    print $ isConnectedGraph graph4


