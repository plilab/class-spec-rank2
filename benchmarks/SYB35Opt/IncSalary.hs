{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--pipe-once #-}

module SYB35Opt.IncSalary where

import Data.Company
import Data.Data3
import Data.List
import Data.Typeable (Proxy)

incProxy :: Proxy IncSalary'
incProxy = undefined

class (Data₃ IncSalary' a) => IncSalary' a where
    incSalary₇ :: Float -> a -> a
    incSalary₇ k = gmapT₃ incProxy (incSalary₇ k)

instance IncSalary' Salary where
    incSalary₇ k (S x) = S (x * (1 + k))

instance IncSalary' Company

instance IncSalary' Dept

instance IncSalary' Name

instance IncSalary' Char

instance IncSalary' String'

instance IncSalary' Float

instance IncSalary' (List' Dept)

instance IncSalary' SubUnit

instance IncSalary' (List' SubUnit)

instance IncSalary' Person

instance IncSalary' Employee
