{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Data.Morphism.Invotomorph.Language where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Typeable

import Data.Morphism.Language

data Auto = Auto Isotype
    deriving (Data, Typeable, Show, Eq)

data QRule = QRule LHS RHS
    deriving (Data, Typeable, Show, Eq)

data RDef = RDef Auto [QRule]
    deriving (Data, Typeable, Show)

instance Lift QRule where
  lift (QRule l r) = do
    lh <- lift l
    rh <- lift r
    return (AppE (AppE (ConE 'QRule) (lh)) (rh))
