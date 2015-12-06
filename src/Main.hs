{-# OPTIONS_GHC -Wall -O2 #-}

import System.Console.CmdArgs (cmdArgsRun)
import System.Environment (getArgs, withArgs)
import System.IO

import NGramCrackers.Parsers.Args

main :: IO ()
main = getArgs >>= \args ->  
       (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes >>= optionHandler

