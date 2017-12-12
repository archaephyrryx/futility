{-# LANGUAGE TupleSections #-}

module Util.Tuple.Apply (
    rotate,
    -- * Tuple-Functorial Operations
    act, resp, each, dup, both,
    -- * Left and Right Application
    ($<),
    (>$),
    tup
) where

import Data.Function
import Control.Monad
import Util.Tuple.Rotate
import Data.Tuple.Extra ((***),(&&&),both,first,second)

infixl 1 $<
infixr 1 >$

-- |'act': uncurried version of function application
act :: ((a -> b),a) -> b
act = uncurry ($)

-- |'resp' applies functions to values in a respective fashion
resp :: ((x -> x1),(y -> y1)) -> (x,y) -> (x1,y1)
resp = uncurry (***)

-- |'dup' pairs a value with itself
dup :: a -> (a,a)
dup x = (x,x)

-- |'tap' is a "tuple-map" function for a pair of functions and a single
-- value
each :: (a -> b, a -> c) -> a -> (b, c)
each = uncurry (&&&)

-- | '$<' is the equivalent of '$' for the first-element functor on tuples
--
--  > f$<(x,y) = (f x, y)
--
--  prop> f.g$<x = f$<(g$<x)
--  prop> id$<x = x
($<) :: (a -> b) -> (a, c) -> (b, c)
($<) = first

-- | '>$' is the equivalent of '$' for the second-element functor on tuples
--
--  > f>$(x,y) = (x, f y)
--
--  prop> f.g>$x = f>$(g>$x)
--  prop> x>$id = x
(>$) :: (b -> c) -> (a, b) -> (a, c)
(>$) = second

-- | 'tup': test over uniform pair
tup :: (a -> Bool) -> (a,a) -> Bool
tup p = uncurry (||) . both p
