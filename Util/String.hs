module Util.String where

import Util.Conditional
import Util.List
import Util.Advanced
import Control.Monad (join)



-- | Remove optional delimiters from around a string
undelim :: Char -> Char -> String -> String
undelim o e = full?.hstrip o.estrip e

-- | undelim, but delimiters must be matched
unmatch :: Char -> Char -> String -> String
unmatch o e = \x -> mcond x ((.(==e)).(&&).(==o)) (const const) (const $ const $ const x) x

-- |'unquote' strips an optional leading single-quote
-- and an optional trailing single-quote from a string
unquote :: String -> String
unquote = undelim '\'' '\''

-- |'unbrace' strips an optional leading open-square-bracket
-- and an optional trailing close-square-bracket from a string
unbrace :: String -> String
unbrace = undelim '[' ']'

-- |Head-Strip: Optionally strip a character from the head of a string
hstrip :: Char -> String -> String
hstrip = flip (flip (hcond [].(==)) (const id)) (:)

-- |End-Strip: Optionally strip a character from the end of a string
estrip :: Char -> String -> String
estrip = join.(.(const.const)).flip(lcond [].(==))const

surround :: String -> String -> String -> String
surround pref suff s = pref ++ s ++ suff
