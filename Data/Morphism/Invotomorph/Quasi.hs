{-# Language TemplateHaskell, QuasiQuotes #-}
module Data.Morphism.Invotomorph.Quasi where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Morphism.Invotomorph.Language
--import Data.Morphism.Invotomorph.Language (Term(Unary))
import Data.Morphism.Invotomorph.Parse
import Data.Morphism.Invotomorph.Prim
import Data.Morphism.Invotomorph.Prim (Rule(..), Invotomorph(..))
import Util.Conditional (if_)
import Data.Either

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Morphism.Invotomorph.Language
import Data.Morphism.Invotomorph.Parse

inv :: QuasiQuoter
inv = QuasiQuoter
        { quoteExp = quoteIvExp
        , quoteDec = quoteIvDec
        , quotePat = quoteIvPat
        , quoteType = quoteIvType
        }

fromRight :: Either a b -> b
fromRight x = case x of
                (Left _) -> error "fromRight"
                (Right x) -> x

quoteIvExp :: String -> Q Exp
quoteIvExp = undefined

quoteIvDec :: String -> Q [Dec]
quoteIvDec s = do
  let RDef (Auto (TName t)) xs = fromRight $ invotoParse s
  xss <- lift xs
  xn <- newName "x"
  yn <- newName "y"
  between <- [| (:<->:) |]
  let a = mkName t
  return [ InstanceD [] (AppT (ConT ''Invotomorph) (ConT a))
      [ ValD
          (VarP (mkName "rule"))
          (NormalB
            (SigE
              (AppE
                (AppE
                  (VarE 'map)
                  (LamE [ (ConP 'QRule
                            [ ConP 'LHS [ConP 'Unary [ConP 'CName [VarP xn]]]
                            , ConP 'RHS [ConP 'Unary [ConP 'CName [VarP yn]]]
                            ]
                          )
                        ]
                        (InfixE
                          (Just (SigE (AppE (VarE 'read) (VarE xn)) (ConT a)))
                          between
                          (Just (SigE (AppE (VarE 'read) (VarE yn)) (ConT a)))
                        )
                  )
                )
                xss
              )
              (AppT ListT (AppT (ConT ''Rule) (ConT a)))
            )
          ) [] ] ]

quoteIvPat :: String -> Q Pat
quoteIvPat = undefined
quoteIvType :: String -> Q Type
quoteIvType = undefined
