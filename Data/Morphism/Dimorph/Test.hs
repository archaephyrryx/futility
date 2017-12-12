{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE EmptyCase #-}
module Data.Morphism.Dimorph.Test where

import Data.Morphism.Dimorph.Quasi
import Data.Morphism.Dimorph.Prim
import Data.Morphism.Dimorph.Language
import Data.Morphism.Dimorph.Parse
import Data.Morphism.Dimorph.Alt
import Data.Morphism.Dimorph.Derive
import Data.Morphism.Dimorph.X

[dimorph|
iso X Integer
A <=> 1
C <=> 2
E True <=> 3
E False <=> 4
|]
