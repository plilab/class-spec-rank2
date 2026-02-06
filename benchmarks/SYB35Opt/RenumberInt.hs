{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin ClassSpecRank2 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--iter:100 #-}
{-# OPTIONS_GHC -fplugin-opt ClassSpecRank2:--pipe-once #-}

module SYB35Opt.RenumberInt (renumberInt₇) where

import Control.Monad.State.Strict
import Data.Data3
import Data.Tree
import Data.Typeable

getUnique :: State Int Int
getUnique = do
    u <- get
    modify (+ 1)
    return u

renumberIntProxy :: Proxy RenumberInt
renumberIntProxy = undefined

renumberInt₇ :: Int -> WTree Int Int -> WTree Int Int
renumberInt₇ x y = evalState (renumberInt₇' y) x

class (Data₃ RenumberInt a) => RenumberInt a where
    renumberInt₇' :: a -> State Int a
    renumberInt₇' = gmapM₃ renumberIntProxy renumberInt₇'

instance RenumberInt Int where
    renumberInt₇' _ = getUnique

instance RenumberInt (WTree Int Int)
