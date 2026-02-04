{-# LANGUAGE LambdaCase #-}

module ClassSpecRank2 (plugin) where

import GHC.Plugins
import Pass.Pepsa (pepsa)
import Pass.TypeConstantFold (typeConstantFold)
import Utils

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = purePlugin
        }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install c todo = do
    opts <- parseCommandLineOpts c
    if no_type_fold opts
        then return $ simplify ++ [pepsa opts] ++ todo
        else return $ simplify ++ [pepsa opts] ++ simplify ++ [typeConstantFold opts] ++ todo ++ [typeConstantFold opts] ++ todo
  where
    simplify =
        filter
            ( \case
                CoreDoSimplify{} -> True
                _ -> False
            )
            todo
