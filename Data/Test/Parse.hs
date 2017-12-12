module Data.Test.Parse where

import Language.Haskell.TH
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many)

import Data.Test.Language

def = emptyDef { identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf ":"
               , opLetter = oneOf "::"
               , reservedOpNames = ["::"]
               , reservedNames = []
               }

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

defParse :: Parser Foo
defParse = do m_whiteSpace
              foo <- fooParse
              eof
              return foo

fooParse :: Parser Foo
fooParse = do m_whiteSpace
              a <- (do
                       x <- many conParse
                       return x)
              m_whiteSpace
              m_reserved "::"
              m_whiteSpace
              b <- (do
                       x <- typParse
                       return x)
              return (Foo a b)


typParse :: Parser TName
typParse = TName . mkName <$> m_identifier

conParse :: Parser CName
conParse = CName . mkName <$> m_identifier

testParse :: String -> Either ParseError Foo
testParse = runP defParse () ""
