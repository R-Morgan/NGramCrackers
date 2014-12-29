import System.IO
import Text.ParserCombinators.Parsec (eof)
import NGramCrackers.NGramCrackers
import NGramCrackers.TupleManipulation
import NGramCrackers.ParagraphParsers
import Data.Char (toUpper)
import Data.List (intersperse)
import Control.Monad (liftM)
import Data.Either.Unwrap (fromRight)

main = do
    inHandle   <- openFile "NGramCrackers/story.txt" ReadMode
    outHandle  <- openFile "NGramCrackers/processed.csv" WriteMode
    contents   <- hGetContents inHandle -- contents :: String
    
    case parseParagraph contents of 
         Left e  -> do putStrLn "Error parsing input: "
                       print e

         Right r -> hPutStrLn outHandle "word,count" >> 
                    mapM_ (hPutStrLn outHandle . doubleToCSV) (lexemeCountProfile $ concat r)

    hClose inHandle
    hClose outHandle
