{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module NGramCrackers.Parsers.Args
( 
  optionHandler
, myModes
, exec
) where

import System.Console.CmdArgs
import System.Exit
import Control.Monad (unless, when)

import qualified System.IO    as SIO
import qualified Data.Text.IO as TIO
import qualified Data.Text    as T
import qualified Data.List    as DL (map, concat, concatMap)

import NGramCrackers.DataTypes
import NGramCrackers.Ops.Infixes
import NGramCrackers.Ops.NG
import NGramCrackers.Ops.Pretty
import NGramCrackers.Parsers.Body
import NGramCrackers.Quant.Counts
import NGramCrackers.Quant.Stats
import NGramCrackers.Utilities.List
import NGramCrackers.Utilities.Tuple

{-| Data declaration of record type for programme options -}
data Args = Profile {  wordC     :: Bool
                     , ttr       :: Bool
                     , sentC     :: Bool
                     , sentStats :: Bool
                     , input     :: FilePath
                     , output    :: FilePath
                    } 
            |
            Extract {  input   :: FilePath
                     , output  :: FilePath
                     , lexemes :: Bool       -- word mode
                     , bigram  :: Bool
                     , trigram :: Bool
                     , ngram   :: Int
                     , debug   :: Bool 
                    } deriving (Show, Data, Typeable)

{-| Record of programme's actual flag descriptions-}
profile :: Args
profile =  Profile { wordC = def &= name "wc" &= help "Print word count"
               , ttr   = def &= help "Print type token ration (ttr)"
               , sentC = def &= help "Mean sentences per paragraph"
               , sentStats = def &= help "Stats about sentences"
               , input = def &= typFile &= help "Input file"
               , output = def &= typFile &= help "Output file"
               }

extract :: Args
extract = Extract { input = def &= typFile &= help "Input file"
                  , output = def &= typFile &= help "Output file"
                  , lexemes = def &= name "words" &= help "Word mode"
                  , bigram = def &= help "Bigram mode"
                  , trigram = def &= help "Trigram mode"
                  , ngram   = def &= help "N-gram mode"
                  , debug = def &= help "Debugging mode"
                  }

{-| Takes a set of Args (e.g., myArgs) and causes the program to exit 
    if the user does not supply an input or output file. If the programme
    does not fail, the exec function is called on opts -}
optionHandler :: Args -> IO ()
optionHandler opts@Profile{..} = do
     when (null input) $ SIO.putStrLn "Supply input file" >> exitWith (ExitFailure 1)
     when (null output) $ SIO.putStrLn "Supply output file" >> exitWith (ExitFailure 1)
     exec opts
optionHandler opts@Extract{..} = do
     when (null input)  $ SIO.putStrLn "Supply input file"  >> exitWith (ExitFailure 1)
     when (null output) $ SIO.putStrLn "Supply output file" >> exitWith (ExitFailure 1)
     unless (lexemes || bigram || trigram || (ngram > 3 && ngram < 8) || debug) $ SIO.putStrLn "Supply a mode"  >> exitWith (ExitFailure 1)
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

-- Something sorta like constants.
_PROGRAM_NAME :: String
_PROGRAM_NAME = "NGramCrackers CLI"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.2.6"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "A CLI utility for quantitative text analysis"

_COPYRIGHT :: String
_COPYRIGHT = "(C) Rianna Morgan 2015"

{-| Takes an Args and returns IO actions. 'input' seems to be evaluated on opts
    in SIO.ReadMode and is bound to a name. In a same way, the output file determined
    before some when expressions to determine what to print to file. The file
    handles are then closed. -}
exec :: Args -> IO ()
exec opts@Profile{..} = do
  outHandle <- SIO.openFile output SIO.WriteMode
  contents <- TIO.hGetContents =<< SIO.openFile input SIO.ReadMode -- contents :: T.Text

  when wordC $ TIO.hPutStrLn outHandle $ (T.pack . show . countWords) contents
  -- countWords is a gross, deprecated way of
  -- counting lexemes

  when ttr   $ TIO.hPutStrLn outHandle $ typeTokenRatio contents

  when sentC $ case parseMultiPara contents of
    Left e  -> do SIO.putStrLn "Error parsing input."
                  print e
                                  
    Right r -> TIO.hPutStrLn outHandle $ ps ms where ms = meanSentsPerParagraph r
                                                     ps = T.pack . show

  when sentStats $ case parseMultiPara contents of
    Left e  -> do SIO.putStrLn "Error parsing input."
                  print e
                                  
    Right r ->  TIO.hPutStrLn outHandle "Sentence per paragraph statics" >>
                  TIO.hPutStrLn outHandle (statsFormatter r)

  --SIO.hClose inHandle
  SIO.hClose outHandle

exec opts@Extract{..} = do
  outHandle <- SIO.openFile output SIO.WriteMode
  contents <- TIO.hGetContents =<< SIO.openFile input SIO.ReadMode -- contents :: T.Text

  when lexemes $ case parseMultiPara contents of
    Left e  -> do SIO.putStrLn "Error parsing input: "
                  print e

    Right r -> TIO.hPutStrLn outHandle "word,count" >>
                 formatOutput outHandle (wcMapToList $ wcMap r)

  when bigram $ case parseMultiPara contents of
    Left e  -> do SIO.putStrLn "Error parsing input: "
                  print e

    Right r -> TIO.hPutStrLn outHandle "bigram,count" >>
                 formatOutput outHandle (ngramLister r bigrams)
                                          
  when trigram $ case parseMultiPara contents of
    Left e  -> do SIO.putStrLn "Error parsing input: "
                  print e

    Right r -> TIO.hPutStrLn outHandle "trigram,count" >>
                 formatOutput outHandle (ngramLister r trigrams)

  when (ngram > 3 && ngram < 8) $ case parseMultiPara contents of
    Left e  -> do SIO.putStrLn "Error parsing input: "
                  print e

    Right r -> TIO.hPutStrLn outHandle "trigram,count" >>
                 formatOutput outHandle (ngramLister r $ getTrueNGrams ngram)


  when debug $ case parseMultiPara contents of
    Left e  -> do SIO.putStrLn "Error parsing input: "
                  print e
    Right r -> SIO.putStrLn "Multiparagraph parsing result: " >> print r

  --SIO.hClose inHandle
  SIO.hClose outHandle
