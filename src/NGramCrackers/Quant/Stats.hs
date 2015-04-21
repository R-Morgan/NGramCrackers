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
import NGramCrackers.Utilities.Tuple
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

{- bigramMIRec :: S.Set T.Text -> [[[T.Text]]] -> M.Map T.Text Maybe Double
bigramMIRec s m doc = case M.lookup bg m of
                        Nothing -> insert m bg mi
                        Just -> Nothing where 
                          mi = (snd . bigramMI) bg doc
                          bg = findMin s
-}

bigramMIRecurs :: S.Set T.Text -> [[[T.Text]]] -> [(T.Text, Maybe Double)]
bigramMIRecurs bgSet doc | S.null bgSet       = []
                         | L.null doc         = []
                         | otherwise = (bg, mi) : 
                           bigramMIRecurs newSet doc where 
                           bg = S.findMin bgSet
                           mi = snd $ bigramMI bg doc
                           newSet = S.deleteMin bgSet


bigramMI :: T.Text -> [[[T.Text]]] -> (T.Text, Maybe Double)
bigramMI bg doc = (bg, mutInf) where
                           concattedDoc = concatMap concat doc
                           mutInf = pMI bgFreq pW1 pW2 total
                           wset   = wordSet concattedDoc
                           bset   = bigramSetDoc doc
                           wmap   = wcMap doc
                           bmap   = bigramMap doc
                           total  = (fromIntegral . length) concattedDoc
                           bgWC   = bigramWordsLookup bg wmap
                           pW1    = (/total) <$> (fromIntegral <$> snd' bgWC)
                           pW2    = (/total) <$> (fromIntegral <$> thrd bgWC)
                           bgFreq = M.lookup bg bmap
                           --fmDiv  = fmap (( /total) . fromIntegral)

{-
bigramMIProfile :: [[[T.Text]]] -> M.Map T.Text Double
bigramMIProfile doc = pMI bgFreq pW1 pW2 total where
                        wset   = (wordSet . concatMap concat) doc
                        bset   = bigramSetDoc doc
                        wmap   = wcMap doc
                        bmap   = bigramMap doc
                        total  = (length . concatMap concat) doc
                        bgFreq = M.lookup bigram bmap
                        pW1    = w1c / total
                        pW2    = w2c / total 
                        w1c    = (snd . bigramWordsLookup) bmap
                        w2c    = (thrd . bigramWordsLookup) bmap
                        bigram = S.elemAt 0 bset
                        bigramList = DL.concatMap (extractor . T.unwords) . DL.concat doc
-}

pMI :: Maybe Int -> Maybe Double -> Maybe Double -> Double -> Maybe Double
pMI Nothing _ _ _ = Nothing
pMI _ Nothing _ _ = Nothing
pMI _ _ Nothing _ = Nothing
pMI (Just count) (Just pW1) (Just pW2) total = Just $ pMI' count pW1 pW2 total

{-| Pointwise mutual information score calculation for bigrams
    based on Church and Hanks -}
pMI' :: Int -> Double -> Double -> Double -> Double
pMI' bgFreq pW1 pW2 total = log $ count / (pW1 * pW2 * total)
                              where count    = fromIntegral bgFreq

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

