{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}

module Data.Wrapped where

import Data.Dimorph
import Data.Maybe
import Data.Char
import Util

-- * Typeclasses

-- | The "Hint" class represents wrapped "Int" values
class Hint a where
    val :: a -> Int -- ^ Extract an Int
    unval :: Int -> a -- ^ Encapsulate an Int
    readH :: String -> a -- ^ Read an Int, then encapsulate
    readH = unval.read
    showH :: a -> String -- ^ Extract, then Show an Int
    showH = show.val

-- | The "Abbrev" class represents Data-types that can be printed in a
-- verbose way or an abbreviated way
class Abbrev a where
    short :: String -> a -- ^ Read an abbreviated string
    long :: String -> a -- ^ Read an verbose string
    brief :: a -> String -- ^ Show as an abbreviated string
    verbose :: a -> String -- ^ Show as a verbose string

-- | The "Stringe" class represents wrapped "String" values
class Stringe a where
    ravel :: String -> a -- ^ Wrap a string
    unravel :: a -> String -- ^ Unwrap a string
    shew :: a -> String -- ^ Unwrap, then show (with inner quotes)
    shew = show.unravel

-- * Implicit Instances

-- | Trivial "Hint" instance for "Int"
instance Hint Int where
    val = id
    unval = id
    readH = read
    showH = show

-- | Trivial "Stringe" instance for "String"
instance Stringe String where
    ravel = id
    unravel = id
    shew = show

-- * Derived Functions

-- | readMaybeH converts an empty string to "Nothing", or an integral
-- string to a "Just" value of a "Hint"-encapsulated "Int"
readMaybeH :: (Hint a) => String -> Maybe a
readMaybeH "" = Nothing
readMaybeH x = Just (readH x)

-- | Adds two "Hint" values
plus :: (Hint a) => a -> a -> a
plus = (unval.).(.val).(+).val

-- | Subtract one "Hint" value from another
minus :: (Hint a) => a -> a -> a
minus = (unval.).(.val).(-).val

-- | Increment a "Hint" value by an Int value
inc :: (Hint a) => Int -> a -> a
inc = (unval.).(.val).(+)

-- | Decrement a "Hint" value by an Int value
dec :: (Hint a) => Int -> a -> a
dec = (unval.).(.val).(+).negate
