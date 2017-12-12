-- | Module for highly specialized utility functions, and those with complex implementations
module Util.Advanced where

import Util.Tuple.Apply
import Util.Tuple (primary, secondary)
import Util.Conditional
import Util.Helper (fromEither)
import Util.List (initLast, headTail, fracture, shatter, headMidLast, permutes)
import Control.Applicative ((<*>),(<$>))

-- |Like 'cond', but with a pre-application after the test
-- >  precond h p f g = cond p (f.h) (g.h)
precond :: (a -> b) -> (a -> Bool) -> (b -> c) -> (b -> c) -> (a -> c)
precond = fcond . flip (.)

-- |Like 'cond', only the conditional function is derived by applying a
-- generator function to a conditional parameter
-- >  fcond h p f g = cond p (h f) (h g)
fcond :: (x -> (a -> b)) -> (a -> Bool) -> x -> x -> (a -> b)
fcond = (curry.).flip((.).uncurry.cond).both

-- |A case of 'fcond' with 'const' as the generator, returning one of
-- two values depending on the result of the test
consd :: (a -> Bool) -> b -> b -> (a -> b)
-- >  consd p x y a = if p a then x else y
consd = fcond const

-- |'condense' maps two functions over a predicate-partitioned list;
--   given a predicate function @p@ and the branch functions @f@ and @g@:
condense :: (a -> Bool) -> (a -> b) -> (a -> c) -> [a] -> ([b],[c])
-- prop> condense p f g == (,) <$> map f.filter p <*>  map g.filter (not.p)
-- Specifically:
-- > condense p f g [] = ([],[])
-- > condense p f g (x:xs) = if_ (p x) ((f x:)$<) (>$(g x:)) $ condense p f g xs
condense = (((`foldr`([], [])).).).flip flip (((>$).(:)).).((.).).(.((($<).(:)).)).cond

-- |'precondense' is a version of 'condense' based on 'precond' instead of 'cond'
precondense :: (a -> b) -> (a -> Bool) -> (b -> c) -> (b -> d) -> [a] -> ([c],[d])
precondense = (((((`foldr`([], [])).).).flip flip (((>$).(:)).).((.).).(.((($<).(:)).))).).precond

-- |'pairings' generates a list of lists of correspondences between elements of two lists
pairings :: [a] -> [b] -> [[(a,b)]]
pairings xs ys
  | length xs == length ys = zip <$> [xs] <*> permutes ys
  | otherwise = []


-- | 'abscond': uses a boolean on a tuple to decide between two curried branches,
--   with a built-in short-circuit based on the outcome of a function returning an Either value
abscond :: ((a0,a1) -> Bool) -> (a0 -> a1 -> b) -> (a0 -> a1 -> b) -> (a -> Either b (a0,a1)) -> a -> b
abscond = ((((.).either id).).).fcond uncurry

-- | Condition on the head of a list to perform a function on the head and tail
hcond :: b -> (a -> Bool) -> (a -> [a] -> b) -> (a -> [a] -> b) -> [a] -> b
hcond z p f g = abscond (primary p) f g (fracture z headTail)

-- | Condition on last to perform a function on both init and last
lcond :: b -> (a -> Bool) -> ([a] -> a -> b) -> ([a] -> a -> b) -> [a] -> b
lcond z p f g = abscond (secondary p) f g (fracture z initLast)

-- | Condition on head and last to perform a function on the head, mid, and last
mcond :: b -> (a -> a -> Bool) -> (a -> [a] -> a -> b) -> (a -> [a] -> a -> b) -> [a] -> b
mcond z p f g = fromEither . shatter z k
  where
    k x x' xs = let (h,ms,l) = headMidLast x x' xs in (if_ (p h l) f g) h ms l
