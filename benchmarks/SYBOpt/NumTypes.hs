{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:1000 #-}

module SYBOpt.NumTypes (numTypes₄) where

import Data.Expr
import Data.Generics

numTypes₄ :: Expr -> Int
numTypes₄ = everything (+) (0 `mkQ` myQ)

myQ :: Type -> Int
myQ _ = 1
