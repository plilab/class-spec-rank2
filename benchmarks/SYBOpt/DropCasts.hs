{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:1000 #-}

module SYBOpt.DropCasts (dropCasts₄) where

import Control.Monad.Writer.Strict
import Data.Expr
import Data.Generics
import Data.Monoid (Sum (..), getSum)

dropCasts₄ :: Expr -> (Expr, Int)
dropCasts₄ x = let (a, b) = runWriter (dropCasts x) in (a, getSum b)

dropCasts :: Expr -> Writer (Sum Int) Expr
dropCasts = everywhereM (return `extM` myExprTrans `extM` myTypeTrans)

myExprTrans :: Expr -> Writer (Sum Int) Expr
myExprTrans (Cast e _) = do
    tell (Sum 1)
    return e
myExprTrans x = return x

myTypeTrans :: Type -> Writer (Sum Int) Type
myTypeTrans (CastTy t _) = do
    tell (Sum 1)
    return t
myTypeTrans x = return x
