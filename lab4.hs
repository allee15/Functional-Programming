--1
factori :: Int -> [Int]
factori n = [x | x<- [1..n], n `mod` x ==0]

--2
prim :: Int ->Bool
prim n = factori n == [1,n]

--3
numerePrime :: Int -> [Int]
numerePrime n = [x | x<- [2..n], prim x]

--4
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 l1 l2 l3 = [ (a,b,c) | (a,(b,c)) <- zip l1 (zip l2 l3)]

--5
firstEl :: [(a,b)] -> [a]
firstEl l = map (\(x,y)->x) l

--6
sumList :: [[Int]] -> [Int]
sumList l = map sum l

--7
prel2 :: [Int] -> [Int]
prel2 l = map(\ x-> if x `mod` 2  == 0 then x `div` 2 else x * 2) l

--8
functie :: Char -> [String] -> [String]
functie x l = filter ( x `elem` ) l

--9
functie2 :: [Int]-> [Int]
functie2 l = map(\ x -> if x `mod` 2 == 1 then x ^ 2 else x ) l

--10
functie3 :: [Int] -> [Int]
functie3 l = map(\(x,y)->x^2)(filter( \(x,y)->odd y)(zip l[1..])) 

--11
eVocala:: Char -> Bool
eVocala c = c`elem` "aeiouAEIOU"

numaiVocale :: [String] -> [String]
numaiVocale l = map(filter eVocala ) l

--12
mymap :: (a -> b) -> [a] -> [b]
mymap fct [] = []
mymap fct (x:xs) = fct x : mymap fct xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter fct [] = []
myfilter fct (x:xs) =
    if fct x
        then x : myfilter fct xs
        else myfilter fct xs
