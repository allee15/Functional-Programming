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