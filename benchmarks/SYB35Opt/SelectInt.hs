{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--pipe-once #-}

module SYB35Opt.SelectInt (SelectInt' (..)) where

import Data.Tree

class SelectInt' a where
    selectInt₇ :: a -> [Int]

instance SelectInt' (WTree Int Int) where
    selectInt₇ (Leaf x) = [x]
    selectInt₇ (Fork l r) = go [] [selectInt₇ l, selectInt₇ r]
    selectInt₇ (WithWeight t w) = go [] [selectInt₇ t, [w]]

go :: [Int] -> [[Int]] -> [Int]
go e [] = e
go e (x : xs) = go (e ++ x) xs
