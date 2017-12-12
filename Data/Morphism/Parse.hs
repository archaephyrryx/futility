module Data.Morphism.Parse where

import Language.Haskell.TH
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many, (<|>))
import Control.Monad (void)

import Data.Morphism.Language

typParse :: Parser Isotype
typParse =
        try tuParse
    <|> try tvParse
    <|> try tbParse
    <|> try ttParse
    <?> "unrecognized type expression"

typParse' :: Parser (VName,VName)
typParse' = vParse`chain`vParse
{-
termParse' :: Parser (Term,Term)
termParse' =
      try (parenth typParse`chain`parenth typParse)
  <|> try (parenth terParse`chain`         typParse)
  <|> try (typParse         `chain`parenth typParse)
  <|> try (tvParse          `chain`         typParse)
  <|> try (ttParse' (tvParse`chain`tvParse) `chain`  typParse)
  <|> try (ttParse          `chain`         typParse)
  <|> try (tbParse          `chain`         typParse)
  <|> try (tuParse          `chain`         typParse)
-}

tuParse, tvParse, tbParse, ttParse :: Parser Isotype
tuParse = Mono <$> ttagParse
tvParse = Poly <$> vParse
tbParse = tbParse' vParse
ttParse = ttParse' typParse'

tbParse' :: Parser VName -> Parser Isotype
tbParse' p = ttagParse >>= \c -> p >>= \t -> return $ Bin c t

ttParse' :: Parser (VName,VName) -> Parser Isotype
ttParse' p = ttagParse >>= \c -> p >>= \(t,s) -> return $ Tern c t s

termParse :: Parser Term
termParse =
      try ternaryParse
  <|> try binaryParse
  <|> try unaryParse
  <|> try varParse
  <|> try strConstParse
  <|> try constParse
  <?> "unrecognized term expression"

atomParse :: Parser Term
atomParse =
      try varParse
  <|> try strConstParse
  <|> try constParse
  <?> "unrecognized atom"

diatomic :: Parser (Term,Term)
diatomic = atomParse`chain`atomParse

whiteout :: Parser a -> Parser a
whiteout p = do { skipMany nbSpace; x <- p; nobreaks; return x; }

nbSpace :: Parser Char
nbSpace = tab <|> char ' '

nobreaks :: Parser ()
nobreaks = do
  manyTill space (lookAhead $ (try (void endOfLine) <|> notFollowedBy nbSpace))
  return ()

parenth :: Parser a -> Parser a
parenth p = between (whiteout.string $ "(") (whiteout.string $ ")") p

chain :: Parser a -> Parser b -> Parser (a,b)
chain p q = whiteout p >>= \x -> whiteout q >>= \y -> return (x,y)

termParse' :: Parser (Term,Term)
termParse' =
      try (parenth termParse`chain`parenth termParse)
  <|> try (parenth termParse`chain`         termParse)
  <|> try (termParse        `chain`parenth termParse)
  <|> try (constParse       `chain`         termParse)
  <|> try (strConstParse    `chain`         termParse)
  <|> try (varParse         `chain`         termParse)
  <|> try (ternaryParse' diatomic `chain`    termParse)
  <|> try (ternaryParse      `chain`         termParse)
  <|> try (binaryParse       `chain`         termParse)
  <|> try (unaryParse        `chain`         termParse)

intParse :: Parser Integer
intParse = do
  digits <- many1 digit
  let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

strParse :: Parser String
strParse = do
  char '"'
  chars <- many strChar
  char '"'
  return $ concat chars
  where
    strChar :: Parser String
    strChar =
          return <$> unescaped
      <|> escaped
      where
        unescaped :: Parser Char
        unescaped = noneOf "\\\"\0\n\r\v\t\b\f"
        escaped :: Parser String
        escaped = do
          s <- char '\\'
          c <- oneOf "\\\"\0\n\r\v\t\b\f"
          return [s,c]


vParse :: Parser VName
vParse = VName . mkName <$> whiteout var_ident
  where
    var_ident :: Parser String
    var_ident = do
      x <- lower
      l <- many (alphaNum <|> oneOf "_'")
      return (x:l)

tagParse :: Parser CName
tagParse = CName . mkName <$> whiteout tag_ident

ttagParse :: Parser TName
ttagParse = TName . mkName <$> whiteout tag_ident

tag_ident :: Parser String
tag_ident = do
  x <- upper
  l <- many (alphaNum <|> oneOf "_'")
  nobreaks
  return (x:l)


constParse, strConstParse, varParse, unaryParse, binaryParse, ternaryParse :: Parser Term
constParse = Constant <$> intParse
strConstParse = StrConst <$> strParse
varParse = Var <$> vParse
unaryParse = Unary <$> tagParse
binaryParse = binaryParse' termParse
ternaryParse = ternaryParse' termParse'

binaryParse' :: Parser Term -> Parser Term
binaryParse' p = do
  c <- tagParse
  x <- whiteout $ p
  return $ Binary c x

ternaryParse' :: Parser (Term,Term) -> Parser Term
ternaryParse' p = do
  c <- tagParse
  (x,y) <- whiteout $ p
  return $ Ternary c x y
