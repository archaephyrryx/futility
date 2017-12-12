{-# Language QuasiQuotes, TemplateHaskell, OverloadedStrings,
    Rank2Types, RankNTypes, ImpredicativeTypes #-}

module Util.Tuple.Variadic where

import Language.Haskell.TH
import Control.Monad

-- Polymorphic monomorphism
rep :: Int -> Q Exp
rep n = do id <- newName "x"
           return $ LamE [VarP id] (TupE $ replicate n $ VarE id)

sel :: Int -> Int -> Q Exp
sel i n = lamE [pat] rhs
    where pat = tupP (map varP as)
          rhs = varE (as !! (i-1))
          as = map mkName ["a" ++ show i | i <- [1..n] ]

tmap i n = do
    f <- newName "f"
    as <- replicateM n (newName "a")
    lamE [varP f, tupP (map varP as)] $
        tupE [ if i == i'
                then [| $(varE f) $a |]
                else a
             | (a,i') <- map varE as `zip` [1..] ]

tamp n = do
    f <- newName "f"
    as <- replicateM n (newName "a")
    lamE [varP f, tupP (map varP as)] $
        tupE [ [| $(varE f) $a |]
             | a <- map varE as ]

zmap n = do
    fs <- replicateM n (newName "f")
    as <- replicateM n (newName "a")
    lamE [tupP (map varP fs), tupP (map varP as)] $
        tupE [ [| $f $a |]
             | (a,f) <- zip (map varE as) (map varE fs) ]
