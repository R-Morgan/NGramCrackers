module NGramCrackers.TextOps (
  bigrams
, trigrams
, getNGramsFromText
, getNGramsFromTextList
, getAlphasOnlyToText
, normToList
, getWordFrequency
) where

import qualified Data.Text as T
import Data.Char (isAlpha, isAlphaNum, isSpace, toLower)
import NGramCrackers.ListManipulation
import NGramCrackers.NGramCrackers
import Data.List (map)

{-| Extract bigrams from a string -}
bigrams :: T.Text -> [T.Text]
bigrams = getNGramsFromText 2 

{-| Extract trigrams from a string -}
trigrams :: T.Text -> [T.Text]
trigrams = getNGramsFromText 3 

{-| Extract n-grams from a string -}
getNGramsFromText :: Int -> T.Text -> [T.Text]
getNGramsFromText n packed -- packed is a packed T.Text string
    | n < 0     = error "n must be a positive integer less than 7"
    | n > 7     = error "n must be a positive integer less than 7"
    | otherwise = map T.unwords $ getNGramsFromTextList n wordList
                    where wordList = T.words packed

getNGramsFromTextList :: Int -> [T.Text] -> [[T.Text]]
getNGramsFromTextList = getNSeqFromList

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnlyToText :: T.Text -> T.Text
getAlphasOnlyToText = (T.filter (\char -> (isAlpha char || isSpace char)))  

{-| Return only alphanumeric characters from a string and return the
    result as a List.-}
normToList :: T.Text -> [T.Text]
normToList = T.words . getAlphasOnlyToText .  T.toLower 

{-| Get frequency of a single word's occurance in a string. Is eta-reduction
    the easiest reading way to do this function? The arguments are fairly
    instructive. However, the type declaration does say what kind of args
    it takes.  With type synonyms or further exploring the type system,
    the declaration would be more informative-}

getWordFrequency:: T.Text -> T.Text -> Int
getWordFrequency word text = (length . filter (== word) . T.words) text

