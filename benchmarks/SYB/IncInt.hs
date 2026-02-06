{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}

module SYB.IncInt (incInt₂) where

import Data.Expr
import Data.Generics

add :: Integer -> Integer
add = (+ 1)

incInt₂ :: Expr -> Expr
incInt₂ = everywhere (mkT add)
