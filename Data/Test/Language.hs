{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Data.Test.Language where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Typeable

newtype TName = TName { t :: Name }
  deriving (Data, Typeable, Show, Eq)

newtype CName = CName { c :: Name }
  deriving (Data, Typeable, Show, Eq)

data Foo = Foo [CName] TName
    deriving (Data, Typeable, Show, Eq)

instance Lift TName where
  lift = return . AppE (ConE 'TName). ConE . t

instance Lift CName where
  lift = return . AppE (ConE 'CName). ConE . c
