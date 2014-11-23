module NGramCrackers.ListManipulation
(
 getNSeqFromList,
 intToList
) where

import Data.List
import Data.Char (isAlpha)

getNGrams :: Int -> [String] -> [[String]]
getNGrams n wordList = getNSeqFromList n wordList 

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnly :: [Char] -> [Char]
getAlphasOnly = (unwords . map (filter (isAlpha)) . words)

{-| Function creates a list of lists of n length -}
getNSeqFromList :: Int -> [a] -> [[a]]
getNSeqFromList n [] = []
getNSeqFromList n (x:xs) 
    | length (x:xs) >= n = take n (x:xs) : getNSeqFromList n xs
    | otherwise = []

intToList :: Integer -> [Int]
intToList i = map read $ words (intersperse ' ' $ show i) :: [Int]
