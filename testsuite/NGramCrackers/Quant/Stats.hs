module NGramCrackers.Quant.Stats
( ttrSet
, pMI
, meanSentLength 
, sdSentLength
, varSentLength
, sentsPerParagraph 
, meanSentsPerParagraph
, sdSentsPerParagraph
, varSentsPerParagraph
) where

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Data.Set as S

import NGramCrackers.Ops.Text
import NGramCrackers.Utilities.List
import NGramCrackers.Parsers.Paragraph
import NGramCrackers.Quant.Dispersion
import NGramCrackers.Quant.Counts

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
pMI bgFreq pW1 pW2 total = log $ count  / (pW1 * pW2 * totCount) 
                             where count = fromIntegral bgFreq
                                   totCount = fromIntegral total
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

