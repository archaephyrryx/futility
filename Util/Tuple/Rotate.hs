{-# LANGUAGE TupleSections #-}
module Util.Tuple.Rotate where

import Data.Tuple.Extra

rotate :: ( (a,b)
          , (c,d) )
       -> ( (a,c)
          , (b,d) )
rotate = (fst *** fst) &&& (snd *** snd)
