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
    inHandle   <- openFile "NGramCrackers/spacelessStory.txt" ReadMode
    outHandle  <- openFile "NGramCrackers/processedStory.txt" WriteMode
    contents <- hGetContents inHandle -- contents :: String
    case parseParagraph contents of 
         Left e  -> do putStrLn "Error parsing input: "
                       print e

         Right r   -> mapM_ (hPutStrLn outHandle . stringifyLexemeCount) (lexemeCountProfile $ concat r)
    hClose inHandle
    hClose outHandle
