{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RecordWildCards       #-}
module Data.Morphism.Dimorph.Prim (
  -- * Dimorph
  --
  -- | The Dimorph datatype is a representation of a Theseus-style
  -- invertible isomorphism built out of pattern-matched rules. One
  -- common use case is when declaring instances of both Read and Show
  -- for a datatype, for which @read.show@ (and @show.read@, by inference)
  -- is equivalent to the identity function. Unlike Invotomorph,
  -- Dimorph is a datatype rather than a typeclass, as it is not an
  -- automorphism and therefore instances would be more difficult to declare
  -- were it a class.
  Dimorph(..),
  -- ** Dimorph Functions
  to, from, maps,

  -- * Mappings
  --
  -- | A Mapping is a data-type representing an atomic rule for
  -- transformation between the two types covered by a Dimorph. Each
  -- mapping simultaneously represents a pattern match on the LHS for the 'to'
  -- transformation and a pattern match on the RHS for the 'from'
  -- transformation. When creating mappings, it is important not to
  -- overlap LHS patterns or RHS patterns in the Mapping-list of a
  -- single Dimorph.
  Mapping(..),

  -- ** Checking and Conversion
  survey, mapping,
  lhs, rhs, consistent
  ) where

import Util
import Data.List (nub)
import Data.Tuple
import Control.Applicative

infix 5 :<=>:

data Mapping a b = a :<=>: b

instance (Eq a, Eq b) => Eq (Mapping a b) where
    (==) (l :<=>: r) (l' :<=>: r') = (l == l' && r == r')

-- | Extracts the LHS from a Mapping
lhs :: Mapping a b -> a
lhs (x :<=>: y) = x

-- | Extracts the RHS from a Mapping
rhs :: Mapping a b -> b
rhs (x :<=>: y) = y

consistent :: (Eq a, Eq b) => [Mapping a b] -> Bool
consistent ms = let (l,r) = each (map lhs, map rhs) ms in nub l == l && nub r == r

-- | The 'survey' conversion resolves a Mapping-list into a pair of
-- functions representing the left-to-right and right-to-left
-- transformations, in that order. Unlike 'mapping', which merely
-- performs the conversion, 'survey' first ensures that the Mappings
-- it is given are all consistent, and then calls 'mapping' after this check. If
-- any Mappings are duplicated in a list, duplicates are removed after
-- the first consistency check fails, and the unique list is then
-- checked for consistency; if this fails, an error is raised.
--
-- The consistency check for Mappings consist of two seperate tests for
-- duplication among the elements of the LHS, and the RHS, of a list of
-- Mappings; if either test finds any duplicates, the check returns
-- False, and otherwise returns True

survey :: (Eq a, Eq b) => [Mapping a b] -> (a -> b) -> (b -> a) -> (a -> b, b -> a)
survey rs | consistent rs = curry $ mapping rs
          | consistent (nub rs) = curry $ mapping (nub rs)
          | otherwise = \_ -> error "Inconsistent"


-- | Resolves a Mapping list into the forward- and
-- backward-tranformation functions; if any pattern isn't matched, an
-- error is returned if a function is applied to it at runtime.
mapping :: (Eq a, Eq b) => [Mapping a b] -> (a -> b, b -> a) -> (a -> b, b -> a)
mapping [] = id
mapping (r@(x :<=>: y):rs) = resp
  (cond (==x) (const y), cond (==y) (const x)).mapping rs

data Dimorph a b = Dimorph { typeX :: [a]
                           , typeY :: [b]
                           , nat :: (a -> b)
                           , inv :: (b -> a)
                           }

instance (Show a, Show b) => Show (Dimorph a b) where
  show = (unlines.) . zipWith (\x y -> show x ++ " <=> " ++ show y) <$> typeX <*> typeY

-- | Generate the to- and from-functions from a Dimorph
dimo :: (Eq a, Eq b) => Dimorph a b -> (a -> b, b -> a)
dimo = survey <$> maps <*> nat <*> inv

-- | Generate the Mapping-list from a Dimorph
maps :: (Eq a, Eq b) => Dimorph a b -> [Mapping a b]
maps = zipWith (:<=>:) <$> typeX <*> typeY

-- | The @a -> b@ side of a Dimorph mapping
to :: (Eq a, Eq b) => Dimorph a b -> (a -> b)
to = fst.dimo

-- | The @b -> a@ side of a Dimorph mapping
from :: (Eq a, Eq b) => Dimorph a b -> (b -> a)
from = snd.dimo
