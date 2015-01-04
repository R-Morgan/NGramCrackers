{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Exit 
import System.IO
import System.Environment (getArgs, getProgName)
import NGramCrackers.NGramCrackers
import NGramCrackers.TupleManipulation
import NGramCrackers.ParagraphParsers
import Data.Char (toUpper)
import Data.List (intersperse)
import Control.Monad (liftM, when)
import System.Console.CmdArgs
import Data.Maybe (fromMaybe)
import Data.List (genericLength, nub)

data Args = Args {  wordC :: Bool
                  , ttr :: Bool
                  , input :: FilePath
                 } deriving (Show, Data, Typeable)

myArgs :: Args
myArgs = Args {  wordC = def &= name "wc" &= help "Print word count"
               , ttr   = def &= help "Print type token ration (ttr)"
               , input = def &= typFile &= help "Input file" }
               &= summary ("NGramCrackers CLI (C) R. Morgan.  The " ++
                           "source code provided here is licenced under " ++
                           "the GPLv 3 or greater. The binary is a command " ++
                           "line tool for quantitative text analysis.")
               &= program "NGramCrackers CLI v0.0.1"

optionHandler :: Args -> IO ()
optionHandler opts@Args{..} = do
     when (not wordC) $ putStrLn "For word count, add -wc"
     when (not ttr)   $ putStrLn "For TTR, add -t"
     when (null input) $ putStrLn "Supply input file" >> exitWith (ExitFailure 1)
     exec opts

exec :: Args -> IO ()
exec opts@Args{..} = do inHandle <- openFile (input) ReadMode 
                        contents <- hGetContents inHandle -- contents :: String
                        when wordC $ putStrLn $ countWords contents
                        when ttr   $ putStrLn $ typeTokenRatio contents
                        hClose inHandle
                        


{- optionHandler Args { wordC = True } = countWords
optionHandler Args { ttr   = True } = typeTokenRatio
optionHandler _ = fullMonty
-}

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

main = cmdArgs myArgs >>= optionHandler

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
