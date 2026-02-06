module Utils where

import Control.Monad
import GHC.Plugins hiding ((<>))
import GHC.Utils.Ppr.Colour
import Text.Read (readMaybe)

data Opts = Opts
    { show_type_fold :: Bool
    , num_iterations :: Int
    , debug :: Bool
    , no_type_fold :: Bool
    , pipe_once :: Bool
    }
    deriving (Show, Eq)

parseCommandLineOpts :: [CommandLineOption] -> CoreM Opts
parseCommandLineOpts [] =
    return
        Opts
            { show_type_fold = False
            , num_iterations = 100
            , debug = False
            , no_type_fold = False
            , pipe_once = False
            }
parseCommandLineOpts (x : xs) =
    do
        opts <- parseCommandLineOpts xs
        case x of
            "--show-type-fold" -> return opts{show_type_fold = True}
            "--verbose" ->
                return
                    opts
                        { show_type_fold = True
                        }
            '-' : '-' : 'i' : 't' : 'e' : 'r' : ':' : xs' ->
                let num_iter = readMaybe xs' :: Maybe Int
                 in case num_iter of
                        Just n -> return opts{num_iterations = n}
                        Nothing -> putMsgS (warn ("Invalid number of iterations for --iter: " ++ xs')) >> return opts
            "--debug" -> return opts{debug = True}
            "--no-type-fold" -> return opts{no_type_fold = True}
            "--pipe-once" -> return opts{pipe_once = True}
            _ -> putMsgS (warn ("Unknown option for ClassSpecRank2: " ++ x)) >> return opts

box :: Int -> String -> PprColour -> String
box width s color =
    let top = renderColour color ++ "┏" ++ replicate (width + 2) '━' ++ "┓\n"
        mid = renderColour color ++ "┃ " ++ renderColour colReset ++ s ++ renderColour color ++ " ┃\n"
        bot = "┗" ++ replicate (width + 2) '━' ++ "┛" ++ renderColour colReset
     in top ++ mid ++ bot

warn :: String -> String
warn s =
    let color = colBold <> colMagentaFg
        reset = colReset
        width = length $ "[WARN]: " ++ s
     in box width ("[" ++ renderColour color ++ "WARN" ++ renderColour reset ++ "]: " ++ s) colMagentaFg

info :: String -> String
info s =
    let color = colBold <> colCyanFg
        reset = colReset
        width = length $ "[INFO]: " ++ s
     in box width ("[" ++ renderColour color ++ "INFO" ++ renderColour reset ++ "]: " ++ s) colCyanFg

success :: String -> String
success s =
    let color = colBold <> colGreenFg
        reset = colReset
        width = length $ "[SUCCESS]: " ++ s
     in box width ("[" ++ renderColour color ++ "SUCCESS" ++ renderColour reset ++ "]: " ++ s) colGreenFg

prt :: forall a. (Outputable a) => a -> CoreM ()
prt = putMsg . ppr

prtIf :: forall a. (Outputable a) => Bool -> a -> CoreM ()
prtIf b x = when b $ prt x

prtSIf :: Bool -> String -> CoreM ()
prtSIf b x = when b $ putMsgS x
