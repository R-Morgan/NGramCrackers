module NGramCrackers.TupleManipulation (
   myArgs
 , optionHandler
 , getOpts
 , exec
) where

import System.Console.CmdArgs

data Args = Args {  wordC  :: Bool
                  , ttr    :: Bool
                  , input  :: FilePath
                  , output :: FilePath
                 } deriving (Show, Data, Typeable)

myArgs :: Args
myArgs = Args {  wordC = def &= name "wc" &= help "Print word count"
               , ttr   = def &= help "Print type token ration (ttr)"
               , input = def &= typFile &= help "Input file"
               , output = def &= typFile &= help "Output file"
               }

optionHandler :: Args -> IO ()
optionHandler opts@Args{..} = do
     when (null input) $ putStrLn "Supply input file" >> exitWith (ExitFailure 1)
     when (null output) $ putStrLn "Supply output file" >> exitWith (ExitFailure 1)
     exec opts


getOpts :: IO Args
getOpts = cmdArgs $ myArgs
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME :: String
_PROGRAM_NAME = "NGramCrackers CLI"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.1.0"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "A CLI utility for quantitative text analysis"

_COPYRIGHT :: String
_COPYRIGHT = "(C) Rianna Morgan 2015"

exec :: Args -> IO ()
exec opts@Args{..} = do inHandle <- openFile (input) ReadMode 
                        outHandle <- openFile (output) WriteMode
                        contents <- hGetContents inHandle -- contents :: String
                        when wordC $ hPutStrLn outHandle $ countWords contents
                        when ttr   $ hPutStrLn outHandle $ typeTokenRatio contents
                        hClose inHandle
                        hClose outHandle

{- 
   case parseParagraph input of 
         Left e  -> do putStrLn "Error parsing input: "
                       print e

         Right r -> hPutStrLn outHandle "word,count" >> 
                    mapM_ (hPutStrLn outHandle . doubleToCSV) (lexemeCountProfile $ concat r)

-}
