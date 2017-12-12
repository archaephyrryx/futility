module Data.TypeCast where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util
import Control.Applicative ((<$>))

sig :: Either Integer Name -> Type -> Exp
sig e t = let e' = showName <$> e
              t' = showName . (\(ConT x) -> x) $ t
           in case t' of
                "String" -> LitE (StringL (restring e'))
                "Char" ->  LitE (CharL (rechar e'))
                "Integer" -> LitE (IntegerL (reinteger e'))
                "Int" -> LitE (IntegerL (reinteger e'))
                _ -> (SigE (ConE (fromRight e)) t)

restring :: Either Integer String -> String
restring = (full?.undelim '"' '"').fromRight

rechar :: Either Integer String -> Char
rechar = only.(full?.undelim '\'' '\'').fromRight

reinteger :: Either Integer String -> Integer
reinteger = either id read
