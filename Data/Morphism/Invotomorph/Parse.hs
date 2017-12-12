module Data.Morphism.Invotomorph.Parse where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many, (<|>))

import Data.Morphism.Invotomorph.Language
import Data.Morphism.Language
import Data.Morphism.Parse


defParse :: Parser RDef
defParse = do m_whiteSpace
              auto <- autoParse
              rules <- many rulingParse
              eof
              return (RDef auto rules)

autoParse :: Parser Auto
autoParse = do m_reserved "auto"
               a <- (do x <- typParse
                        return x)
               return (Auto a)

rulingParse :: Parser QRule
rulingParse = do
                  l <- m_lexeme $ termParse
                  m_lexeme $ m_reservedOp "<->"
                  r <- m_lexeme $ termParse
                  return (QRule (LHS l) (RHS r))

invotoParse :: String -> Either ParseError RDef
invotoParse = runP defParse () ""
