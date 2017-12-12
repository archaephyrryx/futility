module Data.Morphism.Dimorph.Parse where

import Language.Haskell.TH
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many, (<|>))

import Data.Morphism.Dimorph.Language
import Data.Morphism.Language
import Data.Morphism.Parse

defParse :: Parser MDef
defParse = do many space
              iso <- isoParse
              maps <- many (mappingParse)
              eof
              return (MDef iso maps)

isoParse :: Parser Iso
isoParse = do whiteout $ string "iso"
              a <- (do
                       x <- typParse
                       return x)
              b <- (do
                       x <- typParse
                       return x)
              try endOfLine
              return (Iso a b)

mappingParse :: Parser QMapping
mappingParse = do
                  skipMany nbSpace
                  l <- termParse
                  skipMany nbSpace
                  string "<=>"
                  skipMany nbSpace
                  r <- whiteout termParse
                  skipMany nbSpace
                  try endOfLine
                  return $ QMap (LHS l) (RHS r)

dimorphParse :: String -> Either ParseError MDef
dimorphParse = runP defParse () ""
