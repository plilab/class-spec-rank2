{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:1000 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}

module SYBPEOnly.IncInt (incInt₃) where

import Data.Expr
import Data.Generics

add :: Integer -> Integer
add = (+ 1)

incInt₃ :: Expr -> Expr
incInt₃ = everywhere (mkT add)
