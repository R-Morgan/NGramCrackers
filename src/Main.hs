{-# LANGUAGE DeriveDataTypeable #-}

import System.Exit 
import System.IO
import System.Environment (getArgs, getProgName)
import NGramCrackers.NGramCrackers
import NGramCrackers.TupleManipulation
import NGramCrackers.ParagraphParsers
import Data.Char (toUpper)
import Data.List (intersperse)
import Control.Monad (liftM)
import System.Console.CmdArgs
import Data.Maybe (fromMaybe)
import Data.List (genericLength, nub)

data Args = Args {wordC :: Bool, ttr :: Bool} deriving (Show, Data, Typeable)

myArgs = Args {  wordC = def &= name "wc" &= help "Print word count"
               , ttr   = def &= help "Print type token ration (ttr)" }

--optionHandler :: Args
optionHandler Args { wordC = True } = countWords
optionHandler Args { ttr   = True } = typeTokenRatio
optionHandler _ = fullMonty

countWords = show . length . words

typeTokenRatio ::  String -> String
typeTokenRatio string = "Types: " ++ show types ++ ", Tokens: " ++ show tokens ++ ", TTR: "++ show ratio  
                       where types = (genericLength . nub . words) string
                             tokens = (genericLength . words) string
                             ratio = types / tokens

fullMonty :: String -> String
fullMonty tokens = "Count: " ++ count ++ ", " ++ ttr where
                    count = (show . length . words) tokens
                    ttr   = typeTokenRatio tokens

main = cmdArgs myArgs >>= interact . optionHandler

{- do
    argv       <- getArgs 
    progName       <- getProgName
    inHandle       <- openFile "NGramCrackers/story.txt" ReadMode
    outHandle      <- openFile "NGramCrackers/processed.csv" WriteMode
    contents       <- hGetContents inHandle -- contents :: String
    
   case parseParagraph input of 
         Left e  -> do putStrLn "Error parsing input: "
                       print e

         Right r -> hPutStrLn outHandle "word,count" >> 
                    mapM_ (hPutStrLn outHandle . doubleToCSV) (lexemeCountProfile $ concat r)

    hClose inHandle
    hClose outHandle
-}
