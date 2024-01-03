import Text.Parsec
import Text.Parsec.Char
import Data.Char
import Control.Applicative (Alternative, empty)

cifraCuSemn :: Parsec String () Int
cifraCuSemn = do
    ch <- satisfy(\x -> elem x "+-")
    d<-digitToInt <$> digit
    case ch of
        '+' -> return (d)
        '-' -> return (-d)


--scrieti functia de mai sus doar cu applicative, <*>, pure
-- c f -> c v -> c f v
-- pure:: a -> c a
--convert '+' d = d
-- convert '-' d = -d
--pure f <*> arg1<*>arg2...
--f arg1 arg2

--cifraCuSemn = pure convert<*>satisfy(\x -> elem x "+-")<*>convert '+' d = d
                                               -- convert '-' d = -d

--string :: String -> Parser String
--apply (string "Hi") "Hike"
--apply (string "Mai") "Maybe"
ceva ""= return []
ceva (x:xs) = do
    char x
    ceva xs
    return (x:xs)

ceva1 (x:xs) = pure(:)<*> char x <*> ceva xs

instance Alternative Parser where
  empty = Parser (const [])
  p <|> p' = Parser (\input -> case apply p input of
                                 []  -> apply p' input
                                 res -> res)


-- <Nat> :: =<digit><Nat> | <digit>
--nat:: Parser Int
nat = stringToInt<$> naiveNat
    where 
        naiveNat = pure(:)<*> digit<*>naiveNat<|>
                   pure(:[])<*>digit

stringToInt = foldl(\n d -> 108 n + digitToInt d) 0

some :: Alternative f => f a -> f [a]
some v = pure (:) <*> v <*> some v
many v = some v <|> pure []