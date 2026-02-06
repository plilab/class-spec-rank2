{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--no-type-fold #-}

module SYBPEOnly.RenumberInt (renumberInt₃) where

import Control.Monad.State.Strict
import Data.Generics
import Data.Tree

getUnique :: State Int Int
getUnique = do
    u <- get
    modify (+ 1)
    return u

renumberInt₃ :: Int -> WTree Int Int -> WTree Int Int
renumberInt₃ x y = evalState (renumberInt₃' y) x

renumberInt₃' :: WTree Int Int -> State Int (WTree Int Int)
renumberInt₃' = everywhereM (mkM m)

m :: Int -> State Int Int
m _ = getUnique
