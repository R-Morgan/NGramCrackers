module NGramCrackers.NGramCrackers 
( getNGramsFromList
, countNGram
, getNGramFreqs
, ngramCountProfile 
, meanSentLength
, sdSentLength
, varSentLength
, sentsPerParagraph
, meanSentsPerParagraph
, sdSentsPerParagraph
, varSentsPerParagraph
) where

import Data.List (length, nub, sort, concat, concatMap)

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import NGramCrackers.Utilities.List
import NGramCrackers.Parsers.Paragraph
import NGramCrackers.Quant.Dispersion


{-| Extract n-grams from a List. Internal function for n-gram
    string extraction function. Although this looks like a 
    functional synonym for getNSeqFromList, the type signature on
    this function requires that the input list be off StringS
    not just any type -}
getNGramsFromList :: Int -> [String] -> [[String]]
getNGramsFromList = getNSeqFromList 


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

{- Makes a set from a list of words-}
setTypes :: [T.Text] -> S.Set T.Text
setTypes = S.fromList

setTypesDoc :: [[[T.Text]]] -> S.Set T.Text
setTypesDoc = S.fromList . concatMap concat

{-Takes a list of words and calculates a type-token ratio, using Set type to
  to get the length of the unique types. -}
ttrSet :: [T.Text] -> (Double, Double, Double)
ttrSet tokens = (typesTot, tokenTot, ratio)
                   where typesTot = (fromIntegral . S.size . setTypes) tokens
                         tokenTot = (fromIntegral . length) tokens
                         ratio    = typesTot / tokenTot

ttrSet' :: [T.Text] -> (Double, Double, Double)
ttrSet' tokens = (typesTot, tokenTot, ratio)
                   where typesTot = (fromIntegral . S.size . setTypes) tokens
                         tokenTot = (fromIntegral . length) tokens
                         -- Could this be done more efficiently w/Vector?
                         ratio    = typesTot / tokenTot

{-| Takes a parsed paragraph and gets the mean length of the
    sentences in it. -}
meanSentLength :: [[T.Text]] -> Double
meanSentLength paragraph = lengths / sents where
                           lengths = (fromIntegral . sum . map length) paragraph
                           sents   = (fromIntegral . length) paragraph

{-| Takes a parsed paragraph and gets the standard deviation of sentence length
    in it -}
sdSentLength   :: [[T.Text]] -> Double
sdSentLength paragraph = standardDev lengths where
                         lengths = map (fromIntegral . length) paragraph

{-| Takes a parsed paragraph and gets the variance of sentence length in it. -}
varSentLength :: [[T.Text]] -> Double
varSentLength paragraph = variance lengths where
                          lengths = map (fromIntegral . length) paragraph

{-| Takes a paragraph and gets the number of sentence in it. -}
sentsPerParagraph :: [[T.Text]] -> Double
sentsPerParagraph = fromIntegral . length

{-| Takes a list of paragraphs and gets the mean number of sentences per 
    paragrpah. -}
meanSentsPerParagraph :: [[[T.Text]]] -> Double
meanSentsPerParagraph = mean . map sentsPerParagraph

{-| Takes a paragraph and gets the number of sentence in it. -}
sdSentsPerParagraph :: [[[T.Text]]] -> Double
sdSentsPerParagraph = standardDev . map sentsPerParagraph

{-| Takes a paragraph and gets the variance of sentences in it. -}
varSentsPerParagraph :: [[[T.Text]]] -> Double
varSentsPerParagraph = variance . map sentsPerParagraph

