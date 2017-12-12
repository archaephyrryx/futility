module Util.Applicative ( module Control.Applicative
                        , module Util.Applicative
                        )  where

import Control.Applicative

infixl 4 <^>

(<^>) :: Applicative f => f (a -> b) -> a -> f b
f<^>g = ($g) <$> f
