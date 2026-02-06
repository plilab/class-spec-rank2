{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}

module SYB35PEOnly.RmWeights where

import Data.Tree

class RmWeights' a where
    rmWeights₆ :: a -> a

instance RmWeights' (WTree Int Int) where
    rmWeights₆ (WithWeight t _) = rmWeights₆ t
    rmWeights₆ (Leaf x) = Leaf x
    rmWeights₆ (Fork l r) = Fork (rmWeights₆ l) (rmWeights₆ r)
