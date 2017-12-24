module Util.Conditional (
  -- * Booleans and Conditionals
  -- ** Functions
  boolbit, if_, cond,
  -- ** Operators
  (?.), (?:), (.=),
  -- * Conditional Transformer Operators
  -- |The following operators all conditionally apply a given transformation or
  -- the identity transformation based on a property of their second
  -- argument. Here we use the term transformation to mean any function
  -- with the same type signature as 'id', namely @a -> a@.
  (?), (?$), (?<), (?+),
  -- * Functorial Maybe-to-List Operator
  (?/),
  -- * Retrial of failed applications
  -- | The following operators are used to sequence operations in the case that one fails by yielding a 'Nothing' value, allowing different functions to be applied until one succeeds
  (/>>/), (/>|/)
) where

import Control.Applicative ((<*>),(<$>),(<|>))

infix 9 ?
infix 9 ?/
infix 9 ?<
infix 9 ?+
infix 9 ?:
infixr 8 ?$
infixr 8 ?.
infix 9 .=
infixl 9 />>/
infixl 8 />|/

-- |A C-style int-to-bool converter (/=0)
-- Mostly for semantic clarification of C-style int->bool casting
boolbit :: Int -> Bool
boolbit 0 = False
boolbit _ = True

-- |Functional if-then-else (sugar-free)
-- > if_ p x y = if p then x else y
if_ :: Bool -> a -> a -> a
if_ True x _ = x
if_ False _ y = y

-- |A conditional function applicator that avoids unwieldy if-then-else clauses
--
-- prop> foldr (cond p (:) (flip const)) [] = filter p
-- prop> cond id (const a) (const b) x = if x then a else b
--
-- Example: Collatz Snowflake Function
-- > cond even (`div`2) ((+1).(*3))
--
cond :: (a -> Bool) -- ^ Predicate function
     -> (a -> b) -- ^ True branch
     -> (a -> b) -- ^ False branch
     -> (a -> b) -- ^ Resulting function
cond p f g = if_ <$> p <*> f <*> g

-- |Applies a transformation if an evaluated predicate is true, 'id' if false
-- >  p?.f = \x -> if p x then f x else x
(?.) :: (a -> Bool) -> (a -> a) -> (a -> a)
p?.f = cond p f id

-- |Applies a transformation if a predicate is true, 'id' if false
-- >  p?:f = if p then f else id
(?:) :: Bool -> (a -> a) -> (a -> a)
(?:) = (?.).const

-- |Equality test against function result
--
-- prop> (id.=) == (==)
-- prop> (const x.=y) == const (x==y)
(.=) :: Eq b => (a -> b) -> b -> (a -> Bool)
f.=x = (==x).f

-- | Conditional transformer-generator over a Maybe value
-- Given a function that takes a value and returns a transformation,
-- generates a tranformation from a Maybe-enclosed value, defaulting to
-- 'id' in the Nothing case
--
-- prop> _?Nothing = id
-- prop> (const id)?_ = id
--
-- >>> take?Nothing $ "foobar"
-- "foobar"
--
-- >>> take?(Just 3) $ "foobar"
-- "foo"
--
-- >>> drop?(Just 3) $ "foobar"
-- "bar"
(?) :: (a -> (b -> b)) -- ^ Generator for the transformation
    -> Maybe a -- ^ Maybe a value to apply the generator to
    -> (b -> b) -- ^ Transformation
f?Nothing = id
f?(Just x) = f x

-- | Conditional transformer-generator over a boolean predicate function
-- Given a boolean predicate function, applies the transformation
-- generated by a value if the predicate holds true, or the identity
-- transformation if false
(?$) :: (a -> Bool) -- ^ Predicate for conditional branching
     -> (a -> (b -> b)) -- ^ Generator for the transformation
     -> a -- ^ Value to test and conditionally generate on
     -> (b -> b) -- ^ Transformation
p?$f = cond p f (const id)

-- |Conditionally transforms for a zero parameter, 'id' for a nonzero parameter
(?<) :: (a -> a) -> Int -> (a -> a)
f?<0 = f
f?<_ = id

-- |Conditionally transforms if parameter is not one, 'id' if parameter
-- is one
(?+) :: (a -> a) -> Int -> (a -> a)
f?+1 = id
f?+_ = f

-- |Empty list for Nothing, result of application otherwise
(?/) :: (a -> [b]) -> Maybe a -> [b]
f?/Nothing = []
f?/(Just x) = f x

(/>>/) :: (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
(/>>/) f g x = f x <|> g x

(/>|/) :: (a -> Maybe b) -> (a -> b) -> (a -> Maybe b)
(/>|/) f g x = f x <|> (Just $ g x)
