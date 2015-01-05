{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Exit 
import System.IO
import System.Environment (getArgs, getProgName, withArgs)
import NGramCrackers.NGramCrackers
import NGramCrackers.TupleManipulation
import NGramCrackers.ParagraphParsers
import Control.Monad (liftM, when)
import System.Console.CmdArgs
import Data.List (genericLength, nub)

{-| Data declaration of record type for programme options -}
data Args = Args {  wordC  :: Bool  -- word count option
                  , ttr    :: Bool  -- type token ratio option
                  , input  :: FilePath
                  , output :: FilePath
                 } deriving (Show, Data, Typeable)

{-| Record of programme's actual flag descriptions  -}
myArgs :: Args
myArgs = Args {  wordC = def &= name "wc" &= help "Print word count"
               , ttr   = def &= help "Print type token ration (ttr)"
               , input = def &= typFile &= help "Input file"
               , output = def &= typFile &= help "Output file"
               }

main :: IO ()
main = getArgs >>= \args ->  
       (if null args then withArgs ["--help"] else id) getOpts >>= optionHandler

{-| Takes a set of Args (e.g., myArgs) and causes the program to exit 
    if the user does not supply an input or output file. If the programme
    does not fail, the exec function is called on opts -}
optionHandler :: Args -> IO ()
optionHandler opts@Args{..} = do
     when (null input) $ putStrLn "Supply input file" >> exitWith (ExitFailure 1)
     when (null output) $ putStrLn "Supply output file" >> exitWith (ExitFailure 1)
     exec opts

{-| Makes IO args out of myArgs -}
getOpts :: IO Args
getOpts = cmdArgs $ myArgs
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

{-| Takes an Args and returns IO actions. 'input' seems to be evaluated on opts
    in ReadMode and is bound to a name. In a same way, the output file determined
    before some when expressions to determine what to print to file. The file
    handles are then closed. -}
exec :: Args -> IO ()
exec opts@Args{..} = do inHandle <- openFile (input) ReadMode 
                        outHandle <- openFile (output) WriteMode
                        contents <- hGetContents inHandle -- contents :: String
                        when wordC $ hPutStrLn outHandle $ countWords contents
                        when ttr   $ hPutStrLn outHandle $ typeTokenRatio contents
                        hClose inHandle
                        hClose outHandle

-- These functions are the backend of the basic functionalities of 0.1.0
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

{-| These functions output the specified strings, so they can be kept and
 developed separately from the lists that get used in generating the
 the help, version, and about type displays -}
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
