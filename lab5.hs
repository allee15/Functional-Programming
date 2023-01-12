--1
functie1 :: [Int] -> Int
functie1 l = foldr (+) 0 (filter(odd) l)

--2
functie2 :: [Bool] -> Bool
functie2 l = foldr (&&) True l

--3
allVerifies :: (Int -> Bool) -> [Int] ->Bool
allVerifies prop l = foldr(\x acc -> prop x && acc ) True l

--4
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies prop l = foldr(\x acc ->prop x || acc) True l

--5
--mapFoldr :: (a->b) -> [a] -> [b]
--mapFoldr f [] = []
--mapFoldr f [x] = foldr f x [] : []
--mapFoldr f (x:xs)= (foldr f x []) ++ mapFoldr f xs

--6
listToInt :: [Int] -> Int
listToInt l= foldl (\acc x -> acc*10 +x) 0 l

--7 a)
rmChar :: Char->String-> String
rmChar a s = filter(/=a)s

--7b)
rmCharRec :: String -> String -> String
rmCharRec [] b = b
rmCharRec  a [] = []
rmCharRec (x:xa )b=
    if x `elem` b
        then rmCharRec xa ( rmChar x b)
        else rmCharRec xa b

--7c
rmCharFold :: String ->String -> String
rmCharFold a b = foldr rmChar b a