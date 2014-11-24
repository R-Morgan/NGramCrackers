import System.IO
import NGramCrackers.NGramCrackers

main = do
    handle   <- openFile "NGramCrackers/story.txt" ReadMode
    contents <- hGetContents handle 
    print $ bigrams contents
    hClose handle
