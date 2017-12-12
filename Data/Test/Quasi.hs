{-# Language TemplateHaskell, QuasiQuotes #-}
module Data.Test.Quasi where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Test.Language
import Data.Test.Parse
import Util.Conditional (if_)
import Data.Either

test :: QuasiQuoter
test = QuasiQuoter
        { quoteExp = quoteTestE
        , quoteDec = undefined
        , quotePat = undefined
        , quoteType = undefined
        }

quoteTestE :: String -> Q Exp
quoteTestE = qTest . fromRight . testParse

qTest :: Foo -> Q Exp
qTest (Foo [] _) = return (ListE [])
qTest (Foo ((CName c):xt) tn@(TName t)) =
  do
    this <- qCname c t
    rest <- qTest (Foo xt tn)
    return $ InfixE (Just this) (ConE '(:)) (Just rest)

qCname :: Name -> Name -> Q Exp
qCname c t = return $ SigE (ConE c) (ConT t)

fromRight :: Either a b -> b
fromRight x = case x of
                (Left _) -> error "fromRight"
                (Right x) -> x
