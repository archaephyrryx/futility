{-# LANGUAGE RecordWildCards, ImpredicativeTypes #-}
module Data.MicroScope where

import Data.List
import Data.Set (Set)
import Util
import qualified Data.Set as Set

-- |A micro list; a 3-tuple of previous, current, and next elements
type MicroList a = ([a],a,[a])
type Adjustment = forall a. MicroScope a -> MicroScope a
-- |A microscopic list view, consisting of a focused element and all
-- other elements
data MicroScope a = Scanning { hs :: [a], x :: a, ts :: [a] }
                  | Tunneling { xs :: [a], i :: Int }

excise :: Int -> [a] -> MicroList a
excise _ [] = error "Cannot excise empty list"
excise 0 (x:xs) = ([],x,xs)
excise n xs | n < 0 = error "Cannot excise: negative index"
            | n < length xs = let (is,(j:ks)) = splitAt n xs in (reverse is,j,ks)
            | otherwise = error "Cannot excise: index too large"

focus :: MicroScope a -> a
focus s@Scanning{..} = x
focus t@Tunneling{..} = xs !! i

peek :: Adjustment
peek s@Scanning{..} = s
peek t@Tunneling{..} = let (hs,x,ts) = excise i xs in Scanning{..}

dig :: Adjustment
dig s@Scanning{..} = let {xs = reverse hs ++ [x] ++ ts; i = length hs} in Tunneling{..}
dig t@Tunneling{..} = t

isFirst :: MicroScope a -> Bool
isFirst s@Scanning{..} = null hs
isFirst t@Tunneling{..} = i == 0

isLast :: MicroScope a -> Bool
isLast s@Scanning{..} = null ts
isLast t@Tunneling{..} = i == length xs - 1 

goFirst :: Adjustment
goFirst s@Scanning{..} = Scanning [] (last hs) (reverse (init hs) ++ [x] ++ ts)
goFirst t@Tunneling{..} = let i = 0 in Tunneling{..}

goLast :: Adjustment
goLast s@Scanning{..} = Scanning (reverse (init ts) ++ [x] ++ hs) (last ts) []
goLast t@Tunneling{..} = let i = length xs - 1 in Tunneling{..}

goNext :: Adjustment
goNext s@Scanning{..} | null ts = s
                      | otherwise = Scanning (x:hs) (head ts) (tail ts)
goNext t@Tunneling{i=i',..} = let i = cond (<(length xs - 1)) succ id i' in Tunneling{..}

goPrev :: Adjustment
goPrev s@Scanning{..} | null hs = s
                      | otherwise = Scanning (tail hs) (head hs) (x:ts)
goPrev t@Tunneling{i=i',..} = let i = max 0 (pred i') in Tunneling{..}

unscope :: MicroScope a -> MicroList a
unscope s@Scanning{..} = (hs,x,ts)
unscope t@Tunneling{..} = excise i xs

rescopeS :: MicroList a -> MicroScope a
rescopeS (hs,x,ts) = Scanning{..}

rescopeT :: MicroList a -> MicroScope a
rescopeT = dig . rescopeS

reset :: Ord a => Set a -> a -> MicroScope a
reset s x = let (h',t') = Set.split x s
                hs = reverse . Set.toAscList $ h' 
                ts = Set.toAscList $ t'
            in Scanning{..}
