{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:1000 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}

module SYBPEOnly.NumTypes (numTypes₃) where

import Data.Expr
import Data.Generics

numTypes₃ :: Expr -> Int
numTypes₃ = everything (+) (0 `mkQ` my_q)

my_q :: Type -> Int
my_q _ = 1
