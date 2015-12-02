{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -O2 #-}

import System.IO
import System.Environment (getArgs, withArgs)
import System.Console.CmdArgs (cmdArgsRun)

import NGramCrackers.NGramCrackers
import NGramCrackers.Parsers.Args
import NGramCrackers.Parsers.Body
import NGramCrackers.Utilities.Tuple

main :: IO ()
main = getArgs >>= \args ->  
       (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes >>= optionHandler

