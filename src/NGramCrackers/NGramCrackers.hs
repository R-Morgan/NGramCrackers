module NGramCrackers.NGramCrackers 
( countNGram
, getNGramFreqs
, ngramCountProfile 
) where

import Data.List (length, nub, sort, concat, concatMap)

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import NGramCrackers.Ops.Text
import NGramCrackers.Utilities.List
import NGramCrackers.Parsers.Body
import NGramCrackers.Quant.Dispersion
import NGramCrackers.Quant.Counts

{-| Counts the number of times a word occurs in a list of words. Useful
    for counting words in sentences, paragraphs, etc. -}

countNGram :: T.Text -> [T.Text] -> (T.Text, Int) 
countNGram x xs = (x, count) where 
                              count = length $ filter (== x) xs

{-| Gets counts of words in one list from the words in another. This function
    is an internal function used in lexemeCountProfile. A neat feature is that
    with each pass, the list of words to count is reduced through filtering. -}
getNGramFreqs :: [T.Text] -> [T.Text] -> [(T.Text, Int)]
getNGramFreqs _  [] = []
getNGramFreqs [] _  = []
getNGramFreqs (ngram:xs) tokens = countNGram ngram tokens : getNGramFreqs xs newTokens
                                   where newTokens = filter (/= ngram) tokens

{-| Takes a list of words and returns a count of each lexeme's occurance. It
    should be noted that 'types' refers to the list of all words that occur
    in a given text. It is commonly used in applied linguistics to refer to
    type/token ratios to describe the complexity of sentences. -}
ngramCountProfile :: [T.Text] -> [(T.Text, Int)]
ngramCountProfile tokens = getNGramFreqs types tokens 
                              where types = (sort . nub) tokens

{-| -}
typeTokenRatio :: [T.Text] -> (Double, Double, Double)
typeTokenRatio tokens = (typesTotal, tokenTotal, ratio)
                          where typesTotal = (fromIntegral . length . nub) tokens
                                tokenTotal = (fromIntegral . length) tokens
                                ratio      = typesTotal / tokenTotal 
