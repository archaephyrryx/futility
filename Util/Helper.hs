module Util.Helper where

import Control.Monad
import Control.Applicative
import Util.List (for, one)
import Util.Conditional (if_)
import Data.Char
import Util.Generic

-- |The @||@ operator lifted to applicative functors
afor :: (Applicative f) => f Bool -> f Bool -> f Bool
afor = liftA2 (||)

-- |The @&&@ operator lifted to applicative functors
afand :: (Applicative f) => f Bool -> f Bool -> f Bool
afand = liftA2(&&)

-- |Lazy monadic variants of @and@ and @or@
orM, andM :: (Monad m) => m Bool -> m Bool -> m Bool
orM m n = do
  x <- m
  if_ x (return True) n
andM m n = do
  x <- m
  if_ x n (return False)

-- |Ceiling-division for calculating how many bins to use
cdiv :: Int -> Int -> Int
x`cdiv`y | x`mod`y == 0 = x`div`y
         | otherwise = x`div`y + 1

-- |A function that converts paired monads to monadic pairs
mpair :: Monad m => (m a, m b) -> m (a,b)
mpair (mx, my) = do { x <- mx; y <- my; return (x, y) }

-- |A function that converts paired functors to functor pairs
fpair :: Applicative f => (f a, f b) -> f (a,b)
fpair (fx, fy) = (,) <$> fx <*> fy

-- |A 'wrapped' function treats values as singleton lists
--
-- > wrapped f $ x = f [x]
wrapped :: ([a] -> b) -> (a -> b)
wrapped = (.one)

-- |Converts a normal string-parser into a basic Read(er)
rdr :: (String -> a) -> ReadS a
rdr = (wrapped (`zip`[""]) .)

-- | Composes a list of functions of type @a -> a@, effectively sequencially processing them in
-- tail-to-head order
--
-- prop> proc (f:[]) = f
-- prop> proc (f:fs) = f . proc fs
proc :: [a -> a] -> a -> a
proc = foldl1 (.)

-- | Composes a list of generators over the same seed-type for functions of type @a -> a@,
-- effectively sequentially processing them in head-to-tail order over the same seed (see `proc`).
--
-- prop> procmap (map const fs) _ = proc fs
procmap :: [a -> b -> b] -> a -> b -> b
procmap = (proc.).(.flip ($)).for

-- |Constant on unit
{-# INLINE unity #-}
unity :: a -> (() -> a)
unity x () = x


-- |Try to read, avoiding empty string parsing
tryRead :: (Read a) => String -> Maybe a
tryRead "" = Nothing
tryRead x = Just $ read x

-- |Converts a string to titlecase (first character uppercase, all others
-- lowercase)
titleCase :: String -> String
titleCase xs = case xs of
                 [] -> []
                 y:ys -> toUpper y : map toLower ys


-- | Partial function equivalent to (\(Left x) -> x), with informative error messages
fromLeft :: Either a b -> a
fromLeft x = case x of
               (Left x) -> x
               (Right y) -> error $ "fromLeft: (Right "++reveal y++")"


-- | Partial function equivalent to (\(Right x) -> x), with informative error messages
fromRight :: Either a b -> b
fromRight x = case x of
                (Left y) -> error $ "fromRight: (Left "++reveal y++")"
                (Right x) -> x

-- | Non-partial version of `fromLeft` with a lazy default value
getLeft :: a -> Either a b -> a
getLeft ~z x = case x of
                 Left k -> k
                 Right _ -> z

-- | Non-partial version of `fromRight` with a lazy default value
getRight :: b -> Either a b -> b
getRight ~z x = case x of
                  Left _ -> z
                  Right k -> k

-- | Non-partial function that forgetfully maps @Either a a@ to @a@
fromEither :: Either a a -> a
fromEither x = case x of
                 (Left y) -> y
                 (Right y) -> y
