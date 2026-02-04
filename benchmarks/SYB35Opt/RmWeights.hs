{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}

module SYB35Opt.RmWeights where

import Data.Tree

class RmWeights' a where
    rmWeights₇ :: a -> a

instance RmWeights' (WTree Int Int) where
    rmWeights₇ (WithWeight t _) = rmWeights₇ t
    rmWeights₇ (Leaf x) = Leaf x
    rmWeights₇ (Fork l r) = Fork (rmWeights₇ l) (rmWeights₇ r)
