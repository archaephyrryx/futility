{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Data.Morphism.Language where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Data
import Data.Typeable
import Util.TH
import Control.Applicative ((<$>),(<*>))

newtype TName = TName { t :: Name }
  deriving (Data, Typeable, Show, Eq)

newtype CName = CName { c :: Name }
  deriving (Data, Typeable, Show, Eq)

newtype VName = VName { v :: Name }
  deriving (Data, Typeable, Show, Eq)

instance Lift TName where
  lift (TName t) = return $ ConE 'TName $@ ConE t
instance Lift CName where
  lift (CName c) = return $ ConE 'CName $@ ConE c
instance Lift VName where
  lift (VName v) = return $ ConE 'VName $@ ConE v

data Isotype = Poly VName
             | Mono TName
             | Bin TName VName --Isotype
             | Tern TName VName VName --Isotype Isotype
             deriving (Data, Typeable, Show, Eq)

instance Lift Isotype where
  lift (Mono tn) = do
    t <- lift tn
    return (AppE (ConE 'Mono) t)
  lift (Bin tn it) = do
    t <- lift tn
    i <- lift it
    return (AppE (AppE (ConE 'Bin) t) i)
  lift (Tern tn it jt) = do
    t <- lift tn
    i <- lift it
    j <- lift jt
    return (AppE (AppE (AppE (ConE 'Bin) t) i) j)

data Term = Constant Integer
          | StrConst String
          | Var VName
          | Unary CName
          | Binary CName Term
          | Ternary CName Term Term
          deriving (Data, Typeable, Show, Eq)

instance Lift Term where
  lift (Constant i) = (ConE 'Constant $@) <$> lift i
  lift (StrConst i) = (ConE 'StrConst $@) <$> lift i
  lift (Var vn) = (ConE 'Var $@) <$> lift vn
  lift (Unary cn) = (ConE 'Unary $@) <$> lift cn
  lift (Binary cn vn) = (\c v -> ConE 'Binary $@ c $@ v) <$> lift cn <*> lift vn
  lift (Ternary cn xn yn) = (\c x y ->
    ConE 'Ternary $@ c $@ x $@ y) <$> lift cn <*> lift xn <*> lift yn

newtype LHS = LHS { getL :: Term }
    deriving (Data, Typeable, Show, Eq)
instance Lift LHS where
  lift (LHS t) = (ConE 'LHS $@) <$> lift t

newtype RHS = RHS { getR :: Term }
    deriving (Data, Typeable, Show, Eq)
instance Lift RHS where
  lift (RHS t) = (ConE 'RHS $@) <$> lift t
