module Util.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

infixl 2 $@
infixl 2 @:@

($@) :: Exp -> Exp -> Exp
($@) = AppE

(@:@) :: Type -> Type -> Type
(@:@) = AppT
