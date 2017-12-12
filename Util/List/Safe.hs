module Util.List.Safe where

import           System.IO        (hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)

import           Prelude          hiding ((!!))

import           Util.String      (surround)
import           Util.Conditional (if_)

infixl 9 !!


-- | magic : possibly a terrible idea
--
-- gracefully produce an error message, while acting as `id`
magic :: a -> String -> a
magic x incantation = unsafePerformIO $ do
  hPutStrLn stderr $ "Util.List.Safe.magic: \""++incantation++"\""
  return x

(!!) :: [a] -> Int -> a
xs !! n = case xs of
            [] -> error "Util.List.Safe.(!!): failed on empty list"
            x:[] -> if_ (n==0) x $ magic x $ warn n
            x:xt -> case signum n of
                      -1 -> magic x $ warn n
                      0 -> x
                      1 -> xt !! (n-1)

  where
    warn :: Int -> String
    warn = surround "Util.List.Safe.(!!): went " " beyond the bounds of a list" . show
