module NGramCrackers.Ops.String
( bigrams
, trigrams
, getNGramsFromString
, getNGramsFromList
, getAlphasOnlyToString
, getWordFrequency
) where

import Data.Char (isAlphaNum, toLower)

import NGramCrackers.NGramCrackers
import NGramCrackers.Utilities.List

{-| Extract bigrams from a string -}
bigrams :: String -> [String]
bigrams = getNGramsFromString 2 

{-| Extract trigrams from a string -}
trigrams :: String -> [String]
trigrams = getNGramsFromString 3 

{-| Extract n-grams from a string -}
getNGramsFromString :: Int -> String -> [String]
getNGramsFromString n wordString 
    | n < 0     = error "n must be a positive integer less than 7"
    | n > 7     = error "n must be a positive integer less than 7"
    | otherwise = map unwords $ getNGramsFromList n wordList
                    where wordList = getAlphasOnlyToList wordString

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnlyToString :: String -> String
getAlphasOnlyToString = unwords . map (filter isAlphaNum) . words

{-| Return only alphanumeric characters from a string and return the
    result as a List.-}
getAlphasOnlyToList :: String -> [String]
getAlphasOnlyToList = map (filter isAlphaNum) . words . map toLower

{-| Get frequency of a single word's occurance in a string. Is eta-reduction
    the easiest reading way to do this function? The arguments are fairly
    instructive. However, the type declaration does say what kind of args
    it takes.  With type synonyms or further exploring the type system,
    the declaration would be more informative-}

getWordFrequency:: String -> String -> Int
getWordFrequency word text = (length . filter (== word) . words) text

