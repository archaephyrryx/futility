module Data.Accessor where

class (Eq a) => Accessor a b where
  get :: a -> b -> c
  set :: a -> c -> b -> b
  map :: (c -> c) -> a -> b -> b
  map f key x = set key (f $ get key x) x
