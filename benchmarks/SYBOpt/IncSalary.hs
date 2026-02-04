{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}

module SYBOpt.IncSalary (incSalary₄) where

import Data.Company
import Data.Generics

incSalary₄ :: Float -> Company -> Company
incSalary₄ k = everywhere (mkT inc)
  where
    inc :: Salary -> Salary
    inc (S x) = S (x * (1 + k))
