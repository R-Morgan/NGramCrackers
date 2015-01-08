module NGramCrackers.NGramCrackers (
  bigrams
, trigrams
, getNGramsFromString
, getNGramsFromList
, getAlphasOnlyToString
, getAlphasOnlyToList
, getWordFrequency
, countWord
, getWordsFreqs
, lexemeCountProfile 
) where

import Data.Char (isAlpha, toLower)
import NGramCrackers.ListManipulation
import NGramCrackers.ParagraphParsers 
import Data.List (genericLength, nub, sort)

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

{-| Get frequency of a single word's occurance in a string. Is eta-reduction
    the easiest reading way to do this function? The arguments are fairly
    instructive. However, the type declaration does say what kind of args
    it takes.  With type synonyms or further exploring the type system,
    the declaration would be more informative-}

getWordFrequency:: String -> String -> Int
getWordFrequency word text = (length . filter (== word) . words) text

{-|  -}
mapBigrams :: [String] -> [[String]]
mapBigrams = map bigrams

{-| -}
countWord :: String -> [String] -> (String, Int) 
countWord x xs = (x, count) where 
                              count = length $ filter (== x) xs

{-| Gets counts of words in one list from the words in another. This function
    is an internal function used in lexemeCountProfile. A neat feature is that
    with each pass, the list of words to count is reduced through filtering. -}
getWordsFreqs :: [String] -> [String] -> [(String, Int)]
getWordsFreqs _  [] = []
getWordsFreqs [] _  = []
getWordsFreqs (word:xs) tokens = countWord word tokens : getWordsFreqs xs newTokens
                                   where newTokens = filter (/= word) tokens
                                   
{-| Takes a list of words and returns a count of each lexeme's occurance. It
    should be noted that 'types' refers to the list of all words that occur
    in a given text. It is commonly used in applied linguistics to refer to
    type/token ratios to describe the complexity of sentences. -}
lexemeCountProfile :: [String] -> [(String, Int)]
lexemeCountProfile tokens = getWordsFreqs types tokens 
                              where types = (sort . nub) tokens

{-| -}
typeTokenRatio :: [String] -> (Double, Double, Double)
typeTokenRatio tokens = (typesTotal, tokenTotal, ratio)
                          where typesTotal = (genericLength . nub) tokens
                                tokenTotal = genericLength tokens
                                ratio      = typesTotal / tokenTotal 

{-| -}
mapUnwords :: [[String]] -> [String]
mapUnwords  = map unwords

{-| What is this function for again? it seems like a synonym for map -}
mapNGrams :: (String -> [String]) -> [String] -> [[String]]
mapNGrams nGramFunc sents = map nGramFunc sents


