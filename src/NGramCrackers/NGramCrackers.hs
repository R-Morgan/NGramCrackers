module NGramCrackers.NGramCrackers (
  getNGramsFromList
, countNGram
, getNGramFreqs
, ngramCountProfile 
) where

import Data.Char (isAlphaNum, toLower)
import NGramCrackers.ListManipulation
import NGramCrackers.ParagraphParsers 
import Data.List (genericLength, nub, sort)


{-| Extract n-grams from a List. Internal function for n-gram
    string extraction function. Although this looks like a 
    functional synonym for getNSeqFromList, the type signature on
    this function requires that the input list be off StringS
    not just any type -}
getNGramsFromList :: Int -> [String] -> [[String]]
getNGramsFromList = getNSeqFromList 


{-| Counts the number of times a word occurs in a list of words. Useful
    for counting words in sentences, paragraphs, etc. -}
countNGram :: String -> [String] -> (String, Int) 
countNGram x xs = (x, count) where 
                              count = length $ filter (== x) xs

{-| Gets counts of words in one list from the words in another. This function
    is an internal function used in lexemeCountProfile. A neat feature is that
    with each pass, the list of words to count is reduced through filtering. -}
getNGramFreqs :: [String] -> [String] -> [(String, Int)]
getNGramFreqs _  [] = []
getNGramFreqs [] _  = []
getNGramFreqs (ngram:xs) tokens = countNGram ngram tokens : getNGramFreqs xs newTokens
                                   where newTokens = filter (/= ngram) tokens

{-| Takes a list of words and returns a count of each lexeme's occurance. It
    should be noted that 'types' refers to the list of all words that occur
    in a given text. It is commonly used in applied linguistics to refer to
    type/token ratios to describe the complexity of sentences. -}
ngramCountProfile :: [String] -> [(String, Int)]
ngramCountProfile tokens = getNGramFreqs types tokens 
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

