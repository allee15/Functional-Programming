{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}

--1
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap _ Nil= Nil
    fmap f(Cons a os)= Cons ( f a ) (fmap f os)
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    Cons f listF <*> listElem = lappend (fmap f listElem)(listF <*>listElem)
        where lappend Nil list = list
              lappend( Cons a list1) list2 = Cons a(lappend list1 list2)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) -- == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

--2a
data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
-- noNegative a = if a < 0 then Nothing else Just a
noNegative a
 |a < 0 = Nothing
 |otherwise = Just a

test21 = noEmpty "abc" -- == Just "abc"
test22 = noNegative (-5) -- == Nothing
test23 = noNegative 5 -- == Just 5

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString s a w= (Cow)
  <$> noEmpty s -- $ pentru mai multe argumente, * pentru unul.
  <*> noNegative a
  <*> noNegative w
-- cowFromString s a w=
--   fmap(Cow) (noEmpty s) <*> noNegative a <*> noNegative w

test24 = cowFromString "Milka" 5 100 -- == Just (Cow {name = "Milka", age = 5, weight = 100})

--3
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

--a
validateLength :: Int -> String -> Maybe String
validateLength n s = if (length s <= n) then Just s else Nothing

test31 = validateLength 5 "abc" == Just "abc"

--b
mkName :: String -> Maybe Name
mkName s = (Name)
  <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = (Address)
  <$> validateLength 100 s

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

--c
mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = (Person)
  <$> mkName nume
  <*> mkAddress adresa

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))
