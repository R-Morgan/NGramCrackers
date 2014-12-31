import System.IO
import System.Environment (getArgs, getProgName)
import NGramCrackers.NGramCrackers
import NGramCrackers.TupleManipulation
import NGramCrackers.ParagraphParsers
import Data.Char (toUpper)
import Data.List (intersperse)
import Control.Monad (liftM)
import System.Console.GetOpt

-- removed some extraneous imports 


main = do
    argv       <- getArgs 
    progName       <- getProgName
    inHandle       <- openFile "NGramCrackers/story.txt" ReadMode
    outHandle      <- openFile "NGramCrackers/processed.csv" WriteMode
    contents       <- hGetContents inHandle -- contents :: String
    
    case parseParagraph contents of 
         Left e  -> do putStrLn "Error parsing input: "
                       print e

         Right r -> hPutStrLn outHandle "word,count" >> 
                    mapM_ (hPutStrLn outHandle . doubleToCSV) (lexemeCountProfile $ concat r)

    let ( flags, nonOpts, msgs ) = getOpt RequireOrder options argv
    print $ length flags
    
    hClose inHandle
    hClose outHandle

options :: [OptDescr a]
options = []

{- dispatch :: [(String, String -> IO ())]
dispatch = [ ("inf", inf)
           , ("outf", outf)
           ]
-}
