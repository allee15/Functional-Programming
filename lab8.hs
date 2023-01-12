data Punct = Pt [Int]

instance Show Punct where
  show (Pt []) = "()"
  show (Pt l) = "(" ++ afis l ++ ")"
    where 
      afis[]= ""
      afis[x] = show x
      afis (x:y:xs) = show x ++ "," ++ afis(y:xs)


data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a

--instance ToFromArb Punct where


-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

instance (Floating a, Eq a) => Eq (Geo a) 
  where
    fig1 == fig2 == perimeter fig1 == perimeter fig2
