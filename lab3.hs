import Data.Char

--1
palindrom :: String -> Bool
palindrom s = s==reverse s
eVocala:: Char -> Bool 
eVocala c = c `elem` "aeiouAEIOU"
countV :: String -> Int
countV ""=0
countV (c: s)
       | eVocala c = 1+ countV s
       | otherwise = countV s
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (s: ss)
       | palindrom s = countV s + nrVocale ss
       | otherwise = nrVocale ss

--2
f:: Int -> [Int] -> [Int]
--f 0 [1,2,3,4] 
f _ [] = []
f y (x:xs)
        | even x = x : y : f y xs
        | otherwise = x : f y xs

--3
divizori :: Int -> [Int]
divizori n = [d | d<- [1..n], n `mod` d ==0]

--4
listadiv l = [divizori n | n<-l]

--5a
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec x y [] = []
inIntervalRec x y (l:ls) = 
        if x<= l && l<=y
                then l : inIntervalRec x y ls
                else  inIntervalRec x y ls             

--5b
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y l = [ z | z <- l, x<=z && z<=y ]

--6a
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
        |x>0 = 1 + pozitiveRec xs
        | otherwise = pozitiveRec xs


--6b
pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x| x<-l, x>0 ]

--7a
pozitiiImpareRecAux :: [Int] -> Int-> [Int]
pozitiiImpareRecAux [] _ = []
pozitiiImpareRecAux (x:xs) i
                |odd i = i: pozitiiImpareRecAux xs(i+1)
                |otherwise = pozitiiImpareRecAux xs(i+1)
pozitiiImpareRec l = pozitiiImpareRecAux l 0

--7b
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [ i | (i,x) <- zip[0..] l, odd x]

--8a
multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (c:s)
                |isDigit c = digitToInt c * multDigitsRec s
                | otherwise = multDigitsRec s

--8b
multDigitsComp s= product[digitToInt c | c<-s, isDigit c]