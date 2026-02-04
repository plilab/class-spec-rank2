{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}

module SYB35PEOnly.SelectInt (SelectInt' (..)) where

import Data.Tree

class SelectInt' a where
    selectInt₆ :: a -> [Int]

instance SelectInt' (WTree Int Int) where
    selectInt₆ (Leaf x) = [x]
    selectInt₆ (Fork l r) = go [] [selectInt₆ l, selectInt₆ r]
    selectInt₆ (WithWeight t w) = go [] [selectInt₆ t, [w]]

go :: [Int] -> [[Int]] -> [Int]
go e [] = e
go e (x : xs) = go (e ++ x) xs
