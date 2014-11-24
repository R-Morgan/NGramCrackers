module NGramCrackers.NGramCrackers (
  getNGramsFromString
, getNGramsFromList
, getAlphasOnlyToString
, getAlphasOnlyToList
) where

import Data.Char (isAlpha, toLower)
import ListManipulation

bigrams :: [Char] -> [String]
bigrams wordString = getNGramsFromString 2 wordString

trigrams :: [Char] -> [String]
trigrams wordString = getNGramsFromString 3 wordString

getNGramsFromString :: Int -> [Char] -> [String]
getNGramsFromString n wordString = map unwords $ getNGramsFromList n wordList
    where wordList = getAlphasOnlyToList wordString

getNGramsFromList :: Int -> [String] -> [[String]]
getNGramsFromList n wordList = getNSeqFromList n wordList 

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnlyToString :: [Char] -> [Char]
getAlphasOnlyToString = (unwords . map (filter (isAlpha)) . words)

getAlphasOnlyToList :: [Char] -> [String]
getAlphasOnlyToList = (map (map toLower) . map (filter (isAlpha)) . words)

