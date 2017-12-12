module Data.Morphism.Invotomorph.Prim (
    -- * Invotomorph
    --
    -- | The Invotomorph class is designed for ADT's with an inherent symmetry,
    -- with a natural automorphism that is an involution (i.e. its own
    -- inverse). Useful primarily for data types with negatives of many
    -- of its constructors, such as "Bool" and "Int", where the
    -- invotomorphism (not a real term) is boolean negation or
    -- arithmetic negation.
    --
    -- When creating an instance of Invotomorph, it is necessary to
    -- provide either 'classX' and 'classY' definitions, or a definition of 'rule'.
    -- It is usually more convenient to define @classX@ and @classY@, as
    -- those are the LHS and RHS of @rule@ and do not require '(:<->:)' to be imported beforehand
    Invotomorph(..),

    -- * Rule
    --
    -- | A Rule is an atomic bi-directional pattern-match used to
    -- construct the transformation underlying an Invotomorph instance.
    -- If a Rule @A :<->: B@ is an element of a list of Rules, an
    -- invotomorphism derived from that list will map @A@ to @B@ and @B@
    -- to @A@, assuming that their is no conflicting Rule for either @A@
    -- or @B@. In contrast with Mappings of a dimorphism, where
    -- consistency is defined as uniqueness of the elements of the LHS
    -- and the RHS independently, Rules for invotomorphisms must have
    -- uniqueness across the LHS and RHS simultaneously; to illustrate
    -- this more clearly, we will use an example ADT with four
    -- constructors:
    --
    -- @
    --   data Quad = A | B | C | D
    -- @
    --
    -- In this case, it is perfectly fine to define a @Dimorph Quad Quad@
    -- with mappings @[A:<=>:B, B:<=>:C, C:<=>:D, D:<=>:A]@, as a dimorphism is
    -- given as two seperate functions that do pattern matching on the
    -- LHS and RHS respectively and independently, so no patterns are
    -- overlapped; however, if creating an instance @Invoto Quad@, it
    -- would be an error to define this instance in terms of the rules
    -- @[A:<->:B, B:<->:C, C:<->:D, D:<->:A]@ (a valid choice would be
    -- @[A:<->:B, C:<->:D]@), as an invotomorphism is
    -- characterised by a single function that pattern matches on both
    -- the LHS and RHS, so every pattern is duplicated. On a more
    -- fundamental level, the implementations and philosophies of
    -- Invotomorphs and Dimorphs make the distiction clear: as
    -- Invotomorphs are implemented as instances of a typeclass, there
    -- is a single canonical invotomorphism representing an inherent
    -- symmetry in the constructors of a data-type, and therefore an
    -- inherent involution; however, as Dimorph is a data-type, there
    -- can be many different Dimorphs on the same types (even from one
    -- type to itself), each describing a seperate invertible function
    -- from one type to the next, representing a natural parellism
    -- between types rather than a symmetry within a type, without any
    -- canonical Dimorph that governs the types.
    Rule(..),
    -- ** Checking and Conversion
    regulate, ruling
    ) where

import Data.List (nub, nubBy)

infixr 5 :<->:

data Rule a = a :<->: a

equiv :: (Eq a) => Rule a -> Rule a -> Bool
equiv (l:<->:r) (l':<->:r') = (l == l' && r == r') || (l == r' && r == l')


-- | The 'regulate' function performs a checked conversion of
-- Rule-lists to transformations, first checking for consistency of the
-- list of Rules, then proceeding to call 'ruling' if the check passes.
-- If the initial check fails, a second check is performed, after first
-- removing any functionally equivalent rules from the list to check for
-- inconsitent overlaps rather than duplicated overlaps. If the second
-- check passes, the duplicates are removed and the Rules are processed
-- as normal; it if fails, an error is raised.
--
-- Consistency is checked for by ensuring that no pattern matches
-- are present more than once among either side of all of the Rules in
-- a list.
regulate :: (Eq a) => [Rule a] -> (a -> a)
regulate rs | consistent rs = ruling rs
            | consistent . nubBy equiv $ rs = ruling . nubBy equiv $ rs
            | otherwise = error "Inconsistent"

lhs :: Rule a -> a
lhs (x :<->: y) = x

rhs :: Rule a -> a
rhs (x :<->: y) = y

consistent :: (Eq a) => [Rule a] -> Bool
consistent rs = let sides = ((map rhs rs)++(map lhs rs)) in (nub sides) == sides

-- | Resolves a Rule-list into an invotomorphism, which is the identity
-- on any pattern not matched on any side, and the obverse of any
-- pattern for which there a Rule is defined.
ruling :: (Eq a) => [Rule a] -> (a -> a)
ruling [] = id
ruling (r@(x :<->: y):rs) = (\a -> if a == x then y else (if a == y then x else ((ruling rs)$a)))

class (Eq a) => Invotomorph a where
    classX :: [a]
    classX = map lhs rule
    classY :: [a]
    classY = map rhs rule
    invoto :: (a -> a)
    invoto = regulate rule
    rule :: [Rule a]
    rule = zipWith (:<->:) classX classY
    {-# MINIMAL rule | classX, classY #-}
