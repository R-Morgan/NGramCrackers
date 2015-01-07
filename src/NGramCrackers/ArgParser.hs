{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module NGramCrackers.ArgParser(
   optionHandler
 , myModes
 , exec
) where

import System.Console.CmdArgs
import System.IO
import System.Exit
import Control.Monad (when)
import Data.List (genericLength, nub)
import NGramCrackers.ParagraphParsers
import NGramCrackers.NGramCrackers
import NGramCrackers.TupleManipulation

{-| Data declaration of record type for programme options -}
data Args = Profile {  wordC  :: Bool
                     , ttr    :: Bool
                     , input  :: FilePath
                     , output :: FilePath
                    } 
            |
            Extract {  input   :: FilePath
                     , output  :: FilePath
                     , lexemes :: Bool       -- word mode
                    } deriving (Show, Data, Typeable)

{-| Record of programme's actual flag descriptions-}
profile :: Args
profile =  Profile { wordC = def &= name "wc" &= help "Print word count"
               , ttr   = def &= help "Print type token ration (ttr)"
               , input = def &= typFile &= help "Input file"
               , output = def &= typFile &= help "Output file"
               }

extract :: Args
extract = Extract { input = def &= typFile &= help "Input file"
                  , output = def &= typFile &= help "Output file"
                  , lexemes = def &= name "words" &= help "Word mode"
                  , bigrams = def &= help "Bigram mode"
                  }

{-| Takes a set of Args (e.g., myArgs) and causes the program to exit 
    if the user does not supply an input or output file. If the programme
    does not fail, the exec function is called on opts -}
optionHandler :: Args -> IO ()
optionHandler opts@Profile{..} = do
     when (null input) $ putStrLn "Supply input file" >> exitWith (ExitFailure 1)
     when (null output) $ putStrLn "Supply output file" >> exitWith (ExitFailure 1)
     exec opts
optionHandler opts@Extract{..} = do
     when (null input)  $ putStrLn "Supply input file"  >> exitWith (ExitFailure 1)
     when (null output) $ putStrLn "Supply output file" >> exitWith (ExitFailure 1)
     when (not lexemes)   $ putStrLn "Supply a mode"      >> exitWith (ExitFailure 1)
     exec opts

{-| Makes IO args out of myArgs -}
myModes :: Mode (CmdArgs Args) 
myModes = cmdArgsMode $ modes [profile, extract]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME :: String
_PROGRAM_NAME = "NGramCrackers CLI"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.1.1"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "A CLI utility for quantitative text analysis"

_COPYRIGHT :: String
_COPYRIGHT = "(C) Rianna Morgan 2015"

{-| Takes an Args and returns IO actions. 'input' seems to be evaluated on opts
    in ReadMode and is bound to a name. In a same way, the output file determined
    before some when expressions to determine what to print to file. The file
    handles are then closed. -}
exec :: Args -> IO ()
exec opts@Profile{..} = do inHandle <- openFile (input) ReadMode 
                           outHandle <- openFile (output) WriteMode
                           contents <- hGetContents inHandle -- contents :: String
                           when wordC $ hPutStrLn outHandle $ countWords contents
                           when ttr   $ hPutStrLn outHandle $ typeTokenRatio contents
                           hClose inHandle
                           hClose outHandle

exec opts@Extract{..} = do inHandle <- openFile (input) ReadMode 
                           outHandle <- openFile (output) WriteMode
                           contents <- hGetContents inHandle -- contents :: String
                           when lexemes $ 
                             case parseParagraph contents of
                                Left e  -> do putStrLn "Error parsing input: "
                                              print e

                                Right r -> hPutStrLn outHandle "word,count" >> 
                                           mapM_ (hPutStrLn outHandle . doubleToCSV) (lexemeCountProfile $ concat r)
                           hClose inHandle
                           hClose outHandle

-- These functions are the backend of the basic functionalities of 0.1.0
{-| These functions output the specified strings, so they can be kept and
 developed separately from the lists that get used in generating the
 the help, version, and about type displays -}

countWords :: String -> String
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
