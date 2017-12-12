{-# Language TemplateHaskell, QuasiQuotes #-}
module Data.Morphism.Dimorph.Quasi where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.TypeCast
import Data.Morphism.Dimorph.Language
import Data.Morphism.Language
import Data.Morphism.Dimorph.Parse
import Data.Morphism.Dimorph.Alt
import Data.Morphism.Dimorph.Prim
import Util
import Control.Applicative

diExp :: MDef -> Q Exp
diExp m@(MDef i xs) = do
  let (t1, t2) = entype i
      (mexp,nat,inv) = unfoil $ qMDef (t1, t2) xs
  return (SigE (
         VarE 'if_
         $@ ((VarE 'consistent) $@ mexp)
         $@ (ConE 'Dimorph
            $@ (VarE 'map $@ VarE 'lhs $@ mexp)
            $@ (VarE 'map $@ VarE 'rhs $@ mexp)
            $@ nat
            $@ inv
            )
         $@ (VarE 'error
            $@ (LitE (StringL "inconsistency in dimorph definition")))
          ) (ConT ''Dimorph @:@ t1 @:@ t2))

entype :: Iso -> (Type, Type)
entype (Iso x y) = both retype $ (x,y)
  where
    retype :: Isotype -> Type
    retype x = case x of
                 Poly (VName a) -> VarT a
                 Mono (TName t) -> ConT t
                 Bin (TName t) (VName a) -> ConT t @:@ VarT a
                 Tern (TName t) (VName a) (VName b) -> ConT t @:@ VarT a @:@ VarT b

qMDef :: (Type, Type) -> [QMapping] -> (Exp,(Exp,Exp))
qMDef i xs = resp (ListE
                  , both act .
                    foldr (\(a,b) ((f,x),(g,y)) -> ((f,a:x),(g,b:y))) (dup (LamCaseE, [vacuous]))
                  ) $ qPart i xs
  where
    vacuous :: Match
    vacuous = Match WildP (NormalB $ VarE 'error $@ (LitE (StringL "Non-exhaustive mapping in dimorphism"))) []

qPart :: (Type,Type) -> [QMapping] -> ([Exp],[(Match,Match)])
qPart = precondense sides (not.tup exterm.sides) <$> qMapping <*> qMatch

qMatch :: (Type,Type) -> (QTerm, QTerm) -> (Match,Match)
qMatch (t1, t2) (c1,c2) =
    let f = ensig c1 c2 t2
        g = ensig c2 c1 t1
     in (f,g)

qMapping :: (Type, Type) -> (QTerm, QTerm) -> Exp
qMapping (t1,t2) x =
    let (c1,c2) = both icon x
        l = sig c1 t1
        r = sig c2 t2
     in (InfixE (Just l) (ConE '(:<=>:)) (Just r))

data QTerm = QInt Integer
           | QStr String
           | QUni Name
           | QVar Name
           | QBin Name QTerm
           | QTer Name QTerm QTerm
           deriving (Eq, Show)

ensig :: QTerm -> QTerm -> Type -> Match
ensig x y b = Match (enpat x) (NormalB (reexp y b)) []
  where
    enpat :: QTerm -> Pat
    enpat x = case x of
                QUni c -> ConP c []
                QVar v -> VarP v
                QBin c q -> ConP c [enpat q]
                QTer c n m -> ConP c [enpat n, enpat m]
                QStr s -> LitP (StringL s)
                QInt i -> LitP (IntegerL i)
    reexp :: QTerm -> Type -> Exp
    reexp e t = SigE (enexp e) t
      where
        enexp :: QTerm -> Exp
        enexp x = case x of
                    QUni c -> ConE c
                    QVar v -> VarE v
                    QBin c n -> ConE c $@ enexp n
                    QTer c n m -> ConE c $@ enexp n $@ enexp m
                    QStr s -> LitE (StringL s)
                    QInt i -> LitE (IntegerL i)


icon :: QTerm -> Either Integer Name
icon x = case x of
           QInt x -> Left x
           QUni x -> Right x
           QStr s -> Right $ mkName s
           _ -> error "non-QInt/QUni icon"

exterm :: QTerm -> Bool
exterm x = case x of
             (QInt _) -> False
             (QStr _) -> False
             (QUni _) -> False
             _ -> True

magic :: [Name] -> [Name] -> a -> a
magic = magic'
  where
    wiz = const $ error "unused/magic variables"
    magic' [] [] = id
    magic' _ [] = wiz
    magic' [] _ = wiz
    magic' [x] [y] = (x/=y)?:wiz
    magic' xs ys = if_ (any (all (uncurry (==))) $ pairings xs ys) id wiz

sides :: QMapping -> (QTerm, QTerm)
sides (QMap (LHS l) (RHS r)) =
  let ls = map v $ termVarCollect l
      rs = map v $ termVarCollect r
   in magic ls rs $ (qterm l, qterm r)
     where
       qterm :: Term -> QTerm
       qterm x = case x of
                   Constant i -> QInt i
                   StrConst s -> QStr s
                   Unary (CName c) -> QUni c
                   Var (VName x) -> QVar x
                   Binary (CName c) t -> QBin c (qterm t)
                   Ternary (CName c) t t' -> QTer c (qterm t) (qterm t')

quoteDiPat :: String -> Q Pat
quoteDiPat = undefined

quoteDiType :: String -> Q Type
quoteDiType = undefined

dimorph :: QuasiQuoter
dimorph = QuasiQuoter
        { quoteExp = quoteDiExp
        , quoteDec = quoteDiDec
        , quotePat = quoteDiPat
        , quoteType = quoteDiType
        }

biject :: QuasiQuoter
biject = QuasiQuoter
       { quoteExp = undefined
       , quoteDec = bijectDec
       , quotePat = undefined
       , quoteType = undefined
       }

quoteDiExp :: String -> Q Exp
quoteDiExp = diExp . fromRight . dimorphParse

quoteDiDec :: String -> Q [Dec]
quoteDiDec s = do
  let m@(MDef i _) = fromRight $ dimorphParse s
      (t1,t2) = entype i
      nam = mkName ("di" ++ '\'':(showType t1) ++ '\'':(showType t2))
  e <- diExp m
  return [ ValD (VarP nam) (NormalB e) [] ]

showType :: Type -> String
showType (ConT x) = showName x
showType (VarT x) = showName x

bijectDec :: String -> Q [Dec]
bijectDec s =  do
  let m@(MDef i x) = fromRight $ dimorphParse s
      (t1,t2) = entype i
      (e,(a,b)) = qMDef (t1,t2) x
  return [ InstanceD Nothing [] (ConT ''Bijection @:@ t1 @:@ t2)
      [ValD (VarP (mkName "mappings")) (NormalB e) []
      ,ValD (VarP (mkName "matches")) (NormalB (TupE [a,b])) []
      ]
      ]
