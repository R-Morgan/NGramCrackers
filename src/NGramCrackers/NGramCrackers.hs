module NGramCrackers.NGramCrackers (
  getNGramsFromString
, getNGramsFromList
, getAlphasOnlyToString
, getAlphasOnlyToList
) where

import Data.Char (isAlpha)
import ListManipulation

getNGramsFromString :: Int -> [Char] -> [String]
getNGramsFromString n wordString = (map unwords . getNGramsFromList n . words) $ getAlphasOnly wordString

getNGramsFromList :: Int -> [String] -> [[String]]
getNGramsFromList n wordList = getNSeqFromList n wordList 

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnlyToString :: [Char] -> [Char]
getAlphasOnlyToString = (unwords . map (filter (isAlpha)) . words)

getAlphasOnlyToList :: [Char] -> [String]
getAlphasOnlyToList = (map (filter (isAlpha)) . words)

