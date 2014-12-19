import System.IO
--import NGramCrackers.NGramCrackers
import Data.Char (toUpper)

main = do
    inHandle   <- openFile "NGramCrackers/story.txt" ReadMode
    outHandle  <- openFile "NGramCrackers/processedStory.txt" WriteMode
--    contents <- hGetContents inHandle 
    mainLoop inHandle outHandle
    --hPutStrLn outHandle print $ bigrams contents
    hClose inHandle
    hClose outHandle

mainLoop :: Handle -> Handle -> IO ()
mainLoop inHandle outHandle = do isEnd <- hIsEOF inHandle
                                 if isEnd
                                   then return ()
                                   else do inputStr <- hGetLine inHandle
                                           hPutStrLn outHandle (map toUpper inputStr)
                                           mainLoop inHandle outHandle
