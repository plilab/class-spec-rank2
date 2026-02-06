{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}

module SYBPEOnly.IncSalary (incSalary₃) where

import Data.Company
import Data.Generics

incSalary₃ :: Float -> Company -> Company
incSalary₃ k = everywhere (mkT inc)
  where
    inc :: Salary -> Salary
    inc (S x) = S (x * (1 + k))
