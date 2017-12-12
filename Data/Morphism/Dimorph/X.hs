{-# LANGUAGE TemplateHaskell #-}

module Data.Morphism.Dimorph.X where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data X = A
       | C
       | E Bool
       deriving Eq

instance Lift X where
  lift = return . (\x -> case x of A -> ConE 'A; C -> ConE 'C; E t ->  (AppE (ConE 'E) (ConE (if t then 'True else 'False)));)

data Y = B | D deriving (Show, Read, Eq)
