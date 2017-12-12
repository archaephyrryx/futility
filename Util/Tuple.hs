{-# LANGUAGE RankNTypes #-}
module Util.Tuple ( (+++)
                  , knock1, knock2, knock3, knock4, knock5, knock6, knock7
                  , trefoil, unfoil
                  , mhall
                  , swap
                  , repair
                  , primary
                  , secondary
                  , module Util.Tuple.Apply
                  ) where

import Data.Function
import Control.Monad
import Util.Tuple.Apply
import Data.Tuple (swap)

infixr 5 +++

-- * Pair Functions


-- |'repair' takes a lazy pair argument and returns it
{-# INLINE repair #-}
repair :: (a,b) -> (a,b)
repair ~(x,y) = (x,y)

-- |applies a function on the first element of a pair
primary :: forall a c. (forall b. ((a -> c) -> (a, b) -> c))
primary = (.fst)

-- | applies a function on the second element of a pair
secondary :: forall b c. (forall a. ((b -> c) -> (a, b) -> c))
secondary = (.snd)

-- ** The @knock@ Family
-- |'knock1' applies a pair of unary functions
knock1 :: (a0 -> b, a0 -> c) -> a0 -> (b, c)
knock1 = each
-- |'knock2' applies a pair of binary functions
knock2 :: (a0 -> a1 -> b, a0 -> a1 -> c) -> a0 -> a1 -> (b, c)
knock2 = (each.).knock1
-- |'knock3' applies a pair of ternary functions
knock3 :: (a0 -> a1 -> a2 -> b, a0 -> a1 -> a2 -> c) -> a0 -> a1 -> a2 -> (b, c)
knock3 = ((each.).).knock2
knock4 :: (a0 -> a1 -> a2 -> a3 -> b, a0 -> a1 -> a2 -> a3 -> c) -> a0 -> a1 -> a2 -> a3 -> (b, c)
knock4 = (((each.).).).knock3
knock5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> (b, c)
knock5 = ((((each.).).).).knock4
knock6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> (b, c)
knock6 = (((((each.).).).).).knock5
knock7 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> (b, c)
knock7 = ((((((each.).).).).).).knock6

-- *3-tuple Functions

-- |'trefoil' \'folds\' a 3-tuple into an item-pair pair
trefoil :: (a, b, c) -> (a, (b, c))
trefoil ~(x, y, z) = (x, (y, z))

-- |'unfoil' \'unfolds\' an item-pair pair into a 3-tuple
unfoil :: (a, (b, c)) -> (a, b, c)
unfoil ~(x,(y,z)) = (x, y, z)

-- |'mhall' maps a triplet of functions over a triplet of values
mhall :: ((x -> x1),(y -> y1),(z -> z1)) -> (x,y,z) -> (x1,y1,z1)
mhall = (unfoil.).(.trefoil).resp.(resp>$).trefoil

{-# INLINE (+++) #-}
-- |Concatenates corresponding items in a list 3-tuple
(+++) :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
~(d,e,f) +++ ~(g,h,i) = (d++g,e++h,f++i)
