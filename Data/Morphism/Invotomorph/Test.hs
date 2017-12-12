{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Data.Morphism.Invotomorph.Test where

import Data.Morphism.Invotomorph.Quasi
import Data.Morphism.Invotomorph.Prim
import Data.Morphism.Invotomorph.Language
import Data.Morphism.Invotomorph.Parse

data X = A | NotA
       | B | NotB
       | C | NotC
       deriving (Show, Read, Eq)

[inv|
  auto X
    A <-> NotA
    B <-> NotB
    C <-> NotC
  |]
