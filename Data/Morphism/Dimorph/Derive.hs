{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morphism.Dimorph.Derive where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Morphism.Dimorph.Prim hiding (to, from)
import qualified Data.Morphism.Dimorph.Prim as Prim (to, from)
import Data.Morphism.Dimorph.Alt hiding (to, from)
import qualified Data.Morphism.Dimorph.Alt as Alt (to, from)
import Util
import Control.Applicative

instance (Lift a, Lift b) => Lift (Dimorph a b) where
  lift d = do
    x <- lift $ typeX d
    y <- lift $ typeY d
    return $ (AppE (AppE (ConE 'Dimorph) x) y)


deriveRead :: Lift a => Dimorph a String -> Q Type -> Q [Dec]
deriveRead = \d t -> do
  let x = mkName "x"
  d' <- lift d
  t' <- t
  return [InstanceD [] (AppT (ConT ''Read) t')
      [ValD (VarP (mkName "readsPrec")) (NormalB
      (LamE
      [(VarP x)]
      (AppE (AppE (VarE 'const) (AppE (VarE 'rdr) (AppE (VarE 'Prim.from) d'))) (VarE x)))) [] ]]

deriveShow :: Lift a => Dimorph a String -> Q Type -> Q [Dec]
deriveShow = \d t -> do
  let x = mkName "x"
  d' <- lift d
  t' <- t
  return [InstanceD [] (AppT (ConT ''Show) t')
      [ValD (VarP (mkName "show")) (NormalB
      (LamE
      [(VarP x)]
      (AppE (AppE (VarE 'Prim.to) d') (VarE x)))) [] ]]

deriveReadShow :: Lift a => Dimorph a String -> Q Type -> Q [Dec]
deriveReadShow d t = (++) <$> deriveRead d t <*> deriveShow d t

deriveBijection :: (Lift a, Lift b) => Dimorph a b -> Q Type -> Q Type -> Q [Dec]
deriveBijection = \d t1 t2 -> do
  d' <- lift d
  t1' <- t1
  t2' <- t2
  return [ InstanceD [] (AppT (AppT (ConT ''Bijection) t1') t2')
      [ValD (VarP (mkName "di")) (NormalB d') [] ] ]

