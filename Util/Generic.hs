{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-- |Voodoo for @show@ing un@Show@able types
module Util.Generic where

class Reveal a where
  reveal :: a -> String
instance (Visibility a ~ f, Visible f a) => Reveal a where
  reveal = visible (undefined::f)

data HTrue
data HFalse

data Vision a where
  Visionable :: forall a. (Show a) => a -> Vision HTrue
  Unvisionable :: forall a. a -> Vision (Visibility a)

instance Visible HTrue (Vision HTrue) where
  visible _ (Visionable x) = show x
  visible _ (Unvisionable x) = "_"

class Visible (f :: *) (a :: *) where
  visible :: f -> a -> String
instance Show a => Visible HTrue a where
  visible _ = show
instance Visible f a where
  visible _ _ = "_"

instance Visible f a => Visible f (Maybe a) where
  visible _ Nothing = "Nothing"
  visible f (Just x) = "Just "++(visible f x)

type family And a b where
  And HTrue b = b
  And HFalse b = HFalse

type family Or a b where
  Or HTrue b = HTrue
  Or HFalse b = b

type family Visibility a where
  Visibility Int = HTrue
  Visibility Float = HTrue
  Visibility Integer = HTrue
  Visibility Bool = HTrue
  Visibility Char = HTrue
  Visibility [a] = Visibility a
  Visibility (a,b) = And (Visibility a) (Visibility b)
  Visibility (a,b,c) = And (Visibility a) (And (Visibility b) (Visibility c))
  Visibility (a,b,c,d) = And (Visibility a) (And (Visibility b) (And (Visibility c) (Visibility d)))
  Visibility (Vision a) = a
  Visibility a = HFalse

{-
instance TypeCast f HFalse => Visible f a where
  visible _ _ = "_"
  -}



class TypeCast a b | a -> b, b -> a where
  typeCast :: a -> b
class TypeCast' t a b | t a -> b, t b -> a where
  typeCast' :: t -> a -> b
class TypeCast'' t a b | t a -> b, t b -> a where
  typeCast'' :: t -> a -> b
instance TypeCast' () a b => TypeCast a b where
  typeCast = typeCast' ()
instance TypeCast'' t a b => TypeCast' t a b where
  typeCast' = typeCast''
instance TypeCast'' () a a where
  typeCast'' _ x = x
