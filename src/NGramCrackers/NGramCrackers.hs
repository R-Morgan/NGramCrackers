module NGramCrackers.NGramCrackers (
  bigrams
, trigrams
, getNGramsFromString
, getNGramsFromList
, getAlphasOnlyToString
, getAlphasOnlyToList
) where

import Data.Char (isAlpha, toLower)
import NGramCrackers.ListManipulation

{-| Extract bigrams from a string -}
bigrams :: String -> [String]
bigrams = getNGramsFromString 2 

{-| Extract trigrams from a string -}
trigrams :: String -> [String]
trigrams = getNGramsFromString 3 

{-| Extract n-grams from a string -}
getNGramsFromString :: Int -> String -> [String]
getNGramsFromString n wordString = map unwords $ getNGramsFromList n wordList
    where wordList = getAlphasOnlyToList wordString

{-| Extract n-grams from a List. Internal function for n-gram
    string extraction function -}
getNGramsFromList :: Int -> [String] -> [[String]]
getNGramsFromList = getNSeqFromList 

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnlyToString :: String -> String
getAlphasOnlyToString = unwords . map (filter isAlpha) . words

{-| Return only alphabetic characters from a string and return the
    result as a List.-}
getAlphasOnlyToList :: String -> [String]
getAlphasOnlyToList = map (filter isAlpha) . words . map toLower

getNWordFrequency:: String -> String -> Int
getNWordFrequency word text = (length . filter (== word) . words) text
