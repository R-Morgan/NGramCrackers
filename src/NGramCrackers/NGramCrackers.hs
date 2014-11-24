module NGramCrackers.NGramCrackers (
  bigrams
, trigrams
, getNGramsFromString
, getNGramsFromList
, getAlphasOnlyToString
, getAlphasOnlyToList
) where

import Data.Char (isAlpha, toLower)
import ListManipulation

{-| Extract bigrams from a string -}
bigrams :: [Char] -> [String]
bigrams wordString = getNGramsFromString 2 wordString

{-| Extract trigrams from a string -}
trigrams :: [Char] -> [String]
trigrams wordString = getNGramsFromString 3 wordString


{-| Extract n-grams from a string -}
getNGramsFromString :: Int -> [Char] -> [String]
getNGramsFromString n wordString = map unwords $ getNGramsFromList n wordList
    where wordList = getAlphasOnlyToList wordString

{-| Extract n-grams from a List. Internal function for n-gram
    string extraction function -}
getNGramsFromList :: Int -> [String] -> [[String]]
getNGramsFromList n wordList = getNSeqFromList n wordList 

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnlyToString :: [Char] -> [Char]
getAlphasOnlyToString = (unwords . map (filter (isAlpha)) . words)

getAlphasOnlyToList :: [Char] -> [String]
getAlphasOnlyToList = (map (filter (isAlpha)) . words . (map toLower))

