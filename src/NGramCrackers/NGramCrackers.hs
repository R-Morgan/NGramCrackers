module NGramCrackers.NGramCrackers 
( countNGram
, getNGramFreqs
, ngramCountProfile 
, meanSentLength
, sdSentLength
, varSentLength
, sentsPerParagraph
, meanSentsPerParagraph
, sdSentsPerParagraph
, varSentsPerParagraph
, ttrSet
) where

import Data.List (length, nub, sort, concat, concatMap)

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import NGramCrackers.Ops.Text
import NGramCrackers.Utilities.List
import NGramCrackers.Parsers.Paragraph
import NGramCrackers.Quant.Dispersion

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
wordSet :: [T.Text] -> S.Set T.Text
wordSet = S.fromList

{- Makes a set from a docBody -}
wordSetDoc :: [[[T.Text]]] -> S.Set T.Text
wordSetDoc = S.fromList . concatMap concat

ngramSetDoc :: [[[T.Text]]] -> S.Set T.Text
ngramSetDoc = S.fromList . concatMap bigrams . map T.unwords . concat

countWordSetElem :: S.Set T.Text -> [[[T.Text]]] -> [(T.Text, Int)]
countWordSetElem lexSet doc = countWordSetElem' lexSet concattedDoc where
                                concattedDoc = concatMap concat doc
                            
countWordSetElem' :: S.Set T.Text -> [T.Text] -> [(T.Text, Int)]
countWordSetElem' lexSet concattedDoc | S.null lexSet       = []
                                      | L.null concattedDoc = []
                                      | otherwise = (word, count) : 
                                          countWordSetElem' newSet newDoc where 
                                          word = S.elemAt 0 lexSet
                                          count = length $ filter (== word) concattedDoc
                                          newSet = S.deleteAt 0 lexSet
                                          newDoc = filter (/= word) concattedDoc

{-Takes a list of words and calculates a type-token ratio, using Set type to
  to get the length of the unique types. -}
ttrSet :: [T.Text] -> (Double, Double, Double)
ttrSet tokens = (typesTot, tokenTot, ratio)
                   where typesTot = (fromIntegral . S.size . wordSet) tokens
                         tokenTot = (fromIntegral . length) tokens
                         ratio    = typesTot / tokenTot

ttrSet' :: [T.Text] -> (Double, Double, Double)
ttrSet' tokens = (typesTot, tokenTot, ratio)
                   where typesTot = (fromIntegral . S.size . wordSet) tokens
                         tokenTot = (fromIntegral . length) tokens
                         -- Could this be done more efficiently w/Vector?
                         ratio    = typesTot / tokenTot

{- bigramMIProfile :: [[[T.Text]]] -> M.Map T.Text Double
bigramMIProfile doc = pMI bgFreq pW1 pW2 total where
                        bgFreq =
                        bmap   = bigramMap doc
                        wmap   = wcMap doc
                        wset   = (map wordSet . concatMap concat) doc
                        bigram = elemAt 0 wset
-}

{-| Pointwise mutual information score calculation based on Church and Hanks -}
pMI :: Int -> Double -> Double -> Int -> Double
pMI bgFreq pW1 pW2 total = log $ (fromIntegral bgFreq) / (pW1 * pW2 * (fromIntegral total))

{-| Produces Map of bigrams in a document-}
bigramMap :: [[[T.Text]]] -> M.Map T.Text Int
bigramMap doc = bigramMap' stream where
                  bigramMap' = foldl countElem M.empty
                  stream = (concatMap bigrams . map T.unwords . concat) doc

{-| Generalised version of bigramMap -}
ngramMap :: (T.Text -> [T.Text]) -> [[[T.Text]]] -> M.Map T.Text Int
ngramMap f doc = wcMap' stream where
                  wcMap' = foldl countElem M.empty
                  stream = (concatMap f . map T.unwords . concat) doc

{-| Borrowed from: http://nlpwp.org/book/chap-words.xhtml. -}
wcMap :: [[[T.Text]]] -> M.Map T.Text Int
wcMap doc = wcMap' stream where
              wcMap' = foldl countElem M.empty
              stream = concatMap concat doc

{-| Borrowed from: http://nlpwp.org/book/chap-words.xhtml. -}
countElem :: (Ord k) => M.Map k Int -> k -> M.Map k Int
countElem m e = case (M.lookup e m) of
                  Just v  -> M.insert e (v + 1) m
                  Nothing -> M.insert e 1 m

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

