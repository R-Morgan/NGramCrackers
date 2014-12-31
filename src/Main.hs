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

data Flag = Version

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

    --let ( flags, nonOpts, msgs ) = getOpt RequireOrder options argv
    --print $ length flags

    case getOpt RequireOrder options argv of
      (flags, [], []) -> print $ length flags
      ([], nonOpts, []) -> error $ "unrecognised arguments: " ++ unwords nonOpts
      (_, _, errs)      -> error $ concat errs ++ usageInfo header options

    hClose inHandle
    hClose outHandle

options :: [OptDescr Flag]
options = [Option ['v'] ["version"] (NoArg Version) "show version number"]

header :: String
header = "Usage: main [OPTION..]"

{- dispatch :: [(String, String -> IO ())]
dispatch = [ ("inf", inf)
           , ("outf", outf)
           ]
-}
