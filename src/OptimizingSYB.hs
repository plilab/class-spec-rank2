{-# LANGUAGE LambdaCase #-}

module OptimizingSYB (plugin) where

import GHC.Plugins
import Pass.PartialEval (specByPartialEvaluation)
import Pass.Pepsa (pepsa)
import Utils

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install,
      pluginRecompile = purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install c todo = do
  opts <- parseCommandLineOpts c
  if no_symb_exec opts
    then return $ simplify ++ [pepsa opts] ++ todo
    else
      return $ simplify ++ [pepsa opts] ++ simplify ++ [specByPartialEvaluation opts] ++ todo
  where
    simplify =
      filter
        ( \case
            CoreDoSimplify {} -> True
            _ -> False
        )
        todo
