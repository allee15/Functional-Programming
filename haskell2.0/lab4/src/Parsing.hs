
module Parsing where

import Exp
import Lab2
import Control.Applicative (some, many, (<|>))
import Data.Char (isAlpha, isAlphaNum)

parseFirst :: Parser a -> String -> Maybe a
parseFirst p s
  = case apply p s of
      [] -> Nothing
      (a,_):_ -> Just a

haskellId :: Parser String
haskellId = identifier (satisfy isAlpha) (satisfy isAlphaNum)

haskell0p :: Parser String
haskell0p = identifier opSymbol opSymbol
  where 
    opSymbol = satisfy (\x-> elem x "<>?!~+-=|:,.@#$%%^&*")

var :: Parser Var
var = Var <$>  haskellId
-- >>> parseFirst var "b is a var"
-- Just (Var {getVar = "b"})

varExp :: Parser ComplexExp
varExp = CX <$> var 
-- >>> parseFirst varExp "b is a var"
-- Just (CX (Var {getVar = "b"}))

lambdaExp :: Parser ComplexExp
lambdaExp = do
  symbol "\\"
  x <- var
  symbol "->"
  e <- expr
  return $ CLam x e
-- >>> parseFirst lambdaExp "\\x -> x"
-- Just (CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"})))

letExp :: Parser ComplexExp
letExp = do
  symbol "let"
  x <- var
  symbol ":="
  e1 <-expr
  symbol "in"
  e2 <-expr
  return $ Let x e1 e2
-- >>> parseFirst letExp "let x := y in z"
-- Just (Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"})))

letrecExp :: Parser ComplexExp
letrecExp = do
  symbol "letrec"
  x <- var
  symbol ":="
  e1 <- expr
  symbol "in"
  e2 <- expr
  return $ LetRec x e1 e2 
-- >>> parseFirst letrecExp "letrec x := y in z"
-- Just (LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"})))

listExp :: Parser ComplexExp
listExp = List <$> brackets (commaSep expr)
-- >>> parseFirst listExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

natExp :: Parser ComplexExp
natExp = Nat <$> some digit expr
-- >>> parseFirst natExp "223 a"
-- Just (Nat 223)

parenExp :: Parser ComplexExp
parenExp = do
  symbol "("
  e <- expr
  symbol ")"
  return e
-- >>> parseFirst parenExp "(a)"
-- Just (CX (Var {getVar = "a"}))

basicExp :: Parser ComplexExp
basicExp = varExp <|> listExp <|> natExp <|> parenExp
-- >>> parseFirst basicExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

expr :: Parser ComplexExp
expr = varExp
-- >>> parseFirst expr "\\x -> x y z t"
-- Just (CLam (Var {getVar = "x"}) (CApp (CApp (CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y"}))) (CX (Var {getVar = "z"}))) (CX (Var {getVar = "t"}))))

exprParser :: Parser ComplexExp
exprParser = whiteSpace *> expr <* endOfInput
-- >>> parseFirst exprParser "let x := 28 in \\y -> + x y"
-- Just (Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"})))))

