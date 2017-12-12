module Util.List
  ( module Util.List
  , module Util.List.Advanced
  )
  where

import Util.List.Advanced hiding ((!!))
import Util.Conditional
import Data.Functor
import Util.Tuple (repair, unfoil)
import Data.Maybe(fromMaybe)

infix 9 !?
infix 9 ??
infixr 8 ??.
infixl 9 !@

-- |Convenience function to test for non-emptiness
full :: [a] -> Bool
full = not . null

-- |Filters list with simultaneously traversed boolean mask
-- prop> filtrate (map f xs) xs = filter f xs
filtrate :: [Bool] -> [a] -> [a]
filtrate [] _ = []
filtrate _ [] = []
filtrate (p:ps) (x:xs) = (p?:(x:)) $ filtrate ps xs

-- |Safe fetcher-applicator that returns default value for out-of-range indices
--  Lazy on default value, in case it is @undefined@ or @error s@
(!?) :: (a -> b) -> b -> Int -> ([a] -> b)
(f!? ~y) n xs
  | n < 0 = y
  | otherwise = case drop n xs of
                  x:_ -> f x
                  [] -> y

-- |Identity transformation for empty list, generated transformation
-- otherwise (compare with '?')
(??) :: ([a] -> (b -> b)) -> [a] -> (b -> b)
f??[]=id
f??x =f x

-- |Generator for '??'
(??.) :: ([a] -> (b -> b)) -> (c -> [a]) -> c -> (b -> b)
f??.g = (f??).g

-- |'for' is syntactic convenience for 'map' where the function is more
-- complicated than the list; it is purely an argument-flipped 'map'.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- |'one' converts a value to a singleton list
one :: a -> [a]
one = (:[])

-- | 'eno' extracts a value from a singleton list, wrapped in a Maybe
-- monad to allow for empty lists or lists with more than one element
eno :: [a] -> Maybe a
eno = \x -> case x of
              v:[] -> Just v
              _    -> Nothing

-- | assertive version of 'eno', which is undefined on non-singleton lists
only :: [a] -> a
only = fromMaybe err . eno
  where
    err = error "Util.List.only: assertion failed on non-singleton list"


-- |'once' wraps a function result in a singleton list
once :: (a -> b) -> (a -> [b])
once = (one.)

-- |Singleton function over functors, converting @f a@ to a @f [a]@
mono :: Functor f => f a -> f [a]
mono = fmap one

-- |The 'headDef' function returns a default value for an empty list, or
-- the head of a non-empty list
headDef :: a -> [a] -> a
headDef ~x ys = case ys of
                  [] -> x
                  y:_ -> y

-- | Special case of @!?@ for the identity function
get :: a -> Int -> [a] -> a
get x = (id !? x)

-- | unsafe version of 'get' that is based on the assumption that the index is not out of range;
--   result is an error if the index is out of range
unsafeGet :: Int -> [a] -> a
unsafeGet = get (error "Util.List.unsafeGet: index out of range")

-- |Syntactic convenience for second element of a list
neck :: [a] -> a
neck = unsafeGet 1

-- |Syntactic convenience for third element of a list
body :: [a] -> a
body = unsafeGet 2

-- |The 'mmap' function maps over maybe-lists, returning empty for
-- Nothing or the mapped list for Just a list
--
-- prop> mmap f (Just xs) = map f xs
mmap :: (a -> b) -> Maybe [a] -> [b]
mmap = (?/).map

-- | Retrieves multiple indices of a list in one pass.
-- This works as long as the index list is sorted and contains no
-- duplicates: the last element in the resulting list is the last
-- element of the longest sorted, unduplicated, in-range prefix of the
-- index list.
--
-- prop> _!\@[] = []
-- prop> []!\@_ = []
-- prop> xs!\@(one i) = (one!?[]) i xs
-- prop> xs!\@is = xs!\@(tail.filter((==)<$>nub<*>sort).heads$is)
(!@) :: [a] -> [Int] -> [a]
[]!@i = []
x!@[] = []
(x:xs)!@(i:is)
  | i == 0 = x:xs!@map pred is
  | i <  0 = []
  | otherwise = xs!@map pred (i:is)

-- |`shatter`: auxillary function for `headMidLast` for easy casework on sub-singleton/super-singleton lists
shatter :: b -> (a -> a -> [a] -> c) -> [a] -> Either b c
shatter ~z f xs = case xs of
                    [] -> Left z
                    _:[] -> Left z
                    y:y':ys -> Right $ f y y' ys


headMidLast :: a -> a -> [a] -> (a,[a],a)
headMidLast x y zs = unfoil (x, initLast y zs)

-- |`fracture`: auxilliary function for `headTail` and `initLast` for easy casework on empty/non-empty lists
fracture :: b -> (a -> [a] -> c) -> [a] -> Either b c
fracture ~z f xs = case xs of
                  [] -> Left z
                  y:ys -> Right $ f y ys

-- |`headTail`: a fancy way of saying (,) to match the usage and naming pattern of `initLast`
headTail :: a -> [a] -> (a,[a])
headTail = (,)

-- | `initLast`: takes @x@ and @xs@, returns @(init (x:xs),last (x:xs))@ (efficiently)
initLast :: a -> [a] -> ([a],a)
initLast x xs = il xs x
  where
    il :: [a] -> a -> ([a],a)
    --il [] = \x -> ([],x)
    --il (x:xs) = \x0 -> case (il xs) x of ~(zs,z) -> (x0:zs,z)
    il = foldr (\x f x0 -> case f x of ~(zs,z) -> (x0:zs,z)) (\x -> ([],x))
