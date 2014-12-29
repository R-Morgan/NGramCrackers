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

         --Right r -> mapM_ putStrLn $ map stringifyLexemeCount $ (lexemeCountProfile . words . concat) r
         Right r   -> mapM_ putStrLn $ map stringifyLexemeCount $ lexemeCountProfile $ concat r
    --eof

    --hPutStrLn outHandle $ (concat . intersperse "\n" . bigrams) contents
    -- putStrLn $ fromRight $ liftM concat $ liftM (intersperse "\n") $ liftM unwords $ liftM concat $ parseParagraph contents
--    putStrLn $ fromRight $ liftM unwords $ liftM (intersperse "\n") $ liftM concat $ parseParagraph contents
    hClose inHandle
    hClose outHandle

{- mainLoop :: Handle -> Handle -> IO ()
mainLoop inHandle outHandle = do isEnd <- hIsEOF inHandle
                                 if isEnd
                                   then return ()
                                   else do inputStr <- hGetLine inHandle
                                           hPutStrLn outHandle (map toUpper inputStr)
                                           mainLoop inHandle outHandle
-}
