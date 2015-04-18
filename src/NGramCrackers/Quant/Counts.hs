{-# LANGUAGE OverloadedStrings #-}
module NGramCrackers.Quant.Counts
( bigramMap
, ngramMap
, wcMap
, countElem
, wordSet
, wordSetDoc
--, ngramSetDoc
, countWordSetElem
, bigramWordsLookup
, bigramSetDoc
, ngramMap
) where

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as M
import qualified Data.Set   as S
import qualified Data.Text  as T

import NGramCrackers.Ops.Text
import NGramCrackers.Parsers.Paragraph
import NGramCrackers.Quant.Dispersion
import NGramCrackers.Utilities.List


{-| Produces Map of bigrams in a document-}
bigramMap :: [[[T.Text]]] -> M.Map T.Text Int
bigramMap doc = bigramMap' stream where
                  bigramMap' = foldl countElem M.empty
                  stream = (concatMap bigrams . map T.unwords . concat) doc

{-| Looks up counts of a bigrams constituent words in a Map. Useful for the
    calculation of pMI -}
bigramWordsLookup :: T.Text -> M.Map T.Text Int -> (T.Text, Maybe Int, Maybe Int)
-- TODO: Possibly awkward to use tuple format. Possibly not. Evaluate.
-- TODO: Generalise this function to ngrams
bigramWordsLookup bg m = (bg, aC, bC) where
                    wrds = T.words bg
                    aC = M.lookup a m
                    bC = M.lookup b m
                    a    = head wrds
                    b    = last wrds

{-| Looks up bigram's count in a Map. Useful for the calculation of pMI -}
bigramLookup :: T.Text -> M.Map T.Text Int -> (T.Text, Maybe Int)
bigramLookup bg m = (bg, count) where
-- m should be a Map of bigrams and their counts, not just words
                      count = M.lookup bg m

{-| Generalised version of bigramMap -}
ngramMap :: (T.Text -> [T.Text]) -> [[[T.Text]]] -> M.Map T.Text Int
ngramMap f doc = ngramMap' stream where
                   ngramMap' = foldl countElem M.empty
                   stream = (concatMap f . map T.unwords . concat) doc

{-| Borrowed from: http://nlpwp.org/book/chap-words.xhtml. -}
wcMap :: [[[T.Text]]] -> M.Map T.Text Int
wcMap doc = wcMap' stream where
              wcMap' = foldl countElem M.empty
              stream = concatMap concat doc

{-| Borrowed from: http://nlpwp.org/book/chap-words.xhtml. -}
countElem :: (Ord k) => M.Map k Int -> k -> M.Map k Int
countElem m e = case M.lookup e m of
                  Just v  -> M.insert e (v + 1) m
                  Nothing -> M.insert e 1 m

{- Makes a set from a list of words-}
wordSet :: [T.Text] -> S.Set T.Text
wordSet = S.fromList

{- Makes a set from a docBody -}
wordSetDoc :: [[[T.Text]]] -> S.Set T.Text
wordSetDoc = S.fromList . concatMap concat

bigramSetDoc :: [[[T.Text]]] -> S.Set T.Text
bigramSetDoc = S.fromList . concatMap bigrams . map T.unwords . concat

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

test :: [[[T.Text]]]
test = [sents, sents', sents''] where
         sents = map T.words  [ "The quick brown fox jumped over the lazy dog"
                              , "Colourless green ideas sleep furiously"
                              , "Strange watching a VCA film not on purpose"
                              , "Heather Graham is bossy in this movie"
                              , "Each person has ridiculous boundary issues"
                              ]
         sents' = map T.words [ "The quick brown fox jumped over the lazy dog"
                              , "They had very bad behavioural examples as children"
                              , "I can't stand commercials that are too loud"
                              , "There should be laws against that sort of thing"
                              ]

         sents'' = map T.words [ "The quick brown fox jumped over the lazy dog"
                                , "This film is very unrealistic"
                                , "Everyone radiates foolishness"
                                , "Corinne is horrifyingly mean to her children for no apparent reason"
                                , "It probably has to do with outragerously bad parental examples"
                                , "This film in one word sordid"
                                ]
