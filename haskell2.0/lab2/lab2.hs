
import Control.Applicative
import Data.Char
import Text.ParserCombinators.Parsec

newtype Parser a = Parser { apply :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []   -- imposibil de parsat șirul vid
    go (c:input)
      | p c = [(c, input)]   -- dacă predicatul ține, întoarce c și restul șirului de intrare
      | otherwise = []       -- în caz contrar, imposibil de parsat

--- | Acceptă orice caracter
-- isAlphaNumeric -> parser care sa poata parsa doar el. alfanumerice
anychar :: Parser Char
anychar = satisfy (const True)

--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char c = satisfy (==c)

--- | acceptă o cifră
digit :: Parser Char
digit = satisfy isDigit

--- | acceptă un spațiu (sau tab, sau sfârșit de linie -- vedeți funcția din Data.Char )
space :: Parser Char
space = satisfy isSpace

--- | succes doar dacă șirul de intrare este vid 
endOfInput :: Parser ()
endOfInput  = Parser go
  where
    go "" = [((), "")]
    go _ = []

instance Functor Parser where
    fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

--parse :: Parser a -> String -> Either String a
--parse p s = apply  ( p <* endOfInput) s 
  

instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | (a, resta) <- apply pa input, (b, restb) <- apply (k a) resta])

--digitToInt :: Char -> Int
-- apply (digitToInt <$> digit) "7ab"

parseCifra = digitToInt <$> digit
--aplly parseCifra "7ab"

douaCifre c1 c2 = 10*c1 + c2


cifraSemn :: Parser Int
cifraSemn = undefined

string :: String -> Parser String
string = undefined

instance Alternative Parser where
    empty = Parser (const [])
    p <|> p' = Parser (\input -> apply p input ++ apply p' input)

--naiveNatural :: Parser Int
--naiveNatural = pure(:)<*> digit<*>naiveNatural<|>pure(:[])<*>digit






-- | Elimină zero sau mai multe apariții ale lui `space`
whiteSpace :: Parser ()
whiteSpace = () <$ many space

--stringToInt = foldl(\n d -> 10*n + digitToInt d) 0
-- | parses a natural number (one or more digits)
nat :: Parser Int
nat = read <$> some digit

-- | aplică un parser, și elimină spațiile de după
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

-- | parses a natural number and skips the space after it
natural :: Parser Int
natural = lexeme nat

-- | Parses the string and skips whiteSpace after it
symbol :: String -> Parser String
symbol s = lexeme (string s)

-- | Parses the string, skips whiteSpace, returns unit
reserved :: String -> Parser ()
reserved s = lexeme (string s) *> pure ()

-- | parsează virgulă, eliminând spațiile de după
comma :: Parser ()
comma = lexeme (char ',') *> pure ()







-- | parsează argumentul intre paranteze rotunde
--   elimină spațiile de după paranteze
parens :: Parser a -> Parser a
parens p = do
  symbol "("
  x <- p
  symbol ")"
  spaces
  return x

-- | parsează argumentul intre paranteze pătrate
--   elimină spațiile de după paranteze
brackets :: Parser a -> Parser a
brackets p = between (symbol "[") (symbol "]") p <* spaces



-- | una sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = do
  x <- p
  xs <- many (char ',' >> spaces >> p)
  return (x:xs)

-- | zero sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep :: Parser a -> Parser [a]
commaSep p = commaSep1 p <|> return []

-- | date fiind parsere pentru prima literă si pentru felul literelor următoare
--   scrieți un parser pentru un identificator
ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter = do
  start <- identStart
  rest <- many identLetter
  return (start:rest)

-- | ca mai sus, dar elimină spatiile de după
identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)
  where lexeme p = do
          x <- p
          spaces
          return x

