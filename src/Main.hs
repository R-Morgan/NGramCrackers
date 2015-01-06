{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.IO
import System.Environment (getArgs, withArgs)
import NGramCrackers.NGramCrackers
import NGramCrackers.ArgParser
import NGramCrackers.TupleManipulation
import NGramCrackers.ParagraphParsers

main :: IO ()
main = getArgs >>= \args ->  
       (if null args then withArgs ["--help"] else id) getOpts >>= optionHandler

