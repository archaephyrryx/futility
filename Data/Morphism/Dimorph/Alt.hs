{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Morphism.Dimorph.Alt (
  Bijection(..)
  ) where

import Data.Morphism.Dimorph.Prim (Mapping, mapping, Dimorph(nat,inv), maps)
import Util.Tuple.Apply (each)

class (Eq a, Eq b) => Bijection a b where
  di :: Dimorph a b
  mappings :: [Mapping a b]
  mappings = maps di
  matches :: (a -> b, b -> a)
  matches = each (nat,inv) di
  to :: a -> b
  to x = (fst (mapping mappings matches)) x
  from :: b -> a
  from y = (snd (mapping mappings matches)) y
