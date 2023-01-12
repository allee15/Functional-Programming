poly2:: Double->Double->Double->Double->Double
poly2 a b c x = a* x * x + b* x + c

eeny :: Integer -> String
eeny x
    | even x = "eeny"
    | otherwise = "meeny"


fizzbuzz :: Integer -> String
fizzbuzz x= if mod x 3 == 0 && mod x 5 ==0
               then "FizzBuzz"
               else if mod x 5 ==0
                    then "Buzz"
                    else if mod x 3 == 0
                         then "Fizz"
                         else ""


fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3) 




binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial(n-1)k + binomial(n-1)(k-1)




verifL :: [Int] -> Bool
verifL x = mod(length x) 2 ==0



takefinal :: [Int] -> Int -> [Int]
takefinal l n=
    if length l < n
        then 
--        else drop(length l -n)

--remove::[Int]->Int->[Int]
--remove l n =
--    if length l < n+1
--       then l
 --      else (take(n) l) ++ (drop(n+1)l)

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
--semiPareRec :: [Int] -> [Int]
--semiPareRec [] = []
--semiPareRec (x:xs)= if mod x 2 == 0
--        then ( (div x 2): semiPareRec xs)
--        else semiPareRec xs



myreplicate::Int -> Int -> [Int]

myreplicate 0 v = []
myreplicate n v = (v: myreplicate (n-1) v)


sumImp:: [Int] -> Int
sumImp[] = 0
sumImp (x:xs)= val+ (sumImp xs)
        where
        val= if mod x 2 == 1
            then x
            else 0
 
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:lst) = let
    current= if take 1 h == "A"
        then 1
        else 0
    in current + totalLen lst
