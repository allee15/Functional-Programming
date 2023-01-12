import Data.List
--1
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

--a
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s i) = elem s ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

--b
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia fructe = sum [i | Portocala s i <- fructe, ePortocalaDeSicilia(Portocala s i)]

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

--c
--testViermi :: Fruct -> Bool
--testViermi (Mar s i) = if (i == True ) then True else False
nrMereViermi :: [Fruct] -> Int
nrMereViermi fructe = length[ i | Mar s i <- fructe, i == True]

test_nrMereViermi = nrMereViermi listaFructe == 2

--2
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a
vorbeste :: Animal -> String
--vorbeste  s x = if (s == "Pisica") then "Meow!" else "Woof!"
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

--b
rasa :: Animal -> Maybe String
rasa (Caine nume r) = (Just r)
rasa _ = Nothing 

--3
data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

--a
verifica :: Matrice -> Int -> Bool
verifica (M matrice) nr= foldr(&&)True [(sum linie)==nr | (L linie)<- matrice]

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25

--b
doarPozN :: Matrice -> Int -> Bool
doarPozN (M list) n = foldr (&&) True (map pozitive liniiN)
                        where
                            pozitive (L l) =( filter(>0) l)==l
                            liniiN = filter(\ (L l) -> length l == n) list

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 

--c
corect :: Matrice -> Bool
corect (M[]) = True
corect (M[x]) = True
corect (M(L l1 :(L l2): list)) = length l1 == length l2 && (corect M[L l2]:list)


testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) 
