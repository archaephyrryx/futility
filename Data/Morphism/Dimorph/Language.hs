{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE GADTs              #-}
module Data.Morphism.Dimorph.Language where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Typeable

import Data.Morphism.Language

data Iso = Iso Isotype Isotype
    deriving (Data, Typeable, Show, Eq)

data Compat = Compat
            | Incompat String
            deriving (Show, Read, Eq)

{-
tyVarCollect :: Isotype -> [VName]
tyVarCollect x = case x of
                   Poly x -> [x]
                   Mono _ -> []
                   Bin _ i -> tyVarCollect i
                   Tern _ i j -> (tyVarCollect i) ++ (tyVarCollect j)


tyCompat :: Isotype -> Isotype -> Compat
tyCompat t t' = case (t,t') of
                  (Poly (VName v), Poly (VName v')) ->
                    if v == v'
                       then Incompat "cannot define general `a -> a` dimorphism"
                       else Incompat "magical polymorphism cannot be defined"
                  (Mono _, Mono _) -> Compat
                  (Mono _,
-}

termVarCollect :: Term -> [VName]
termVarCollect x = case x of
                     Var v -> [v]
                     Binary _ a -> termVarCollect a
                     Ternary _ a b -> termVarCollect a ++ termVarCollect b
                     _ -> []


{-
tmCompat :: Term -> Term -> Bool
-}

data QMapping = QMap LHS RHS
    deriving (Data, Typeable, Show, Eq)

data MDef = MDef Iso [QMapping]
    deriving (Data, Typeable, Show)

instance Lift QMapping where
  lift (QMap l r) = do
    lh <- lift l
    rh <- lift r
    return (AppE (AppE (ConE 'QMap) (lh)) (rh))
