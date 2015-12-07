{-# LANGUAGE OverloadedStrings #-}
module NGramCrackers.Quant.Counts
( bigramMap
, ngramMap
, wcMap
, wcMapToList
, countElem
, wordSet
, wordSetDoc
--, ngramSetDoc
, countWordSetElem
, bigramWordsLookup
, bigramSetDoc
, test
) where

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as M
import qualified Data.Set   as S
import qualified Data.Text  as T

import NGramCrackers.DataTypes
import NGramCrackers.Ops.NG
import NGramCrackers.Parsers.Body
import NGramCrackers.Quant.Dispersion
import NGramCrackers.Utilities.List

{-| Produces Map of bigrams in a document-}
bigramMap :: DocCol T.Text -> CrackerMap
bigramMap doc = bigramMap' stream where
                  bigramMap' = foldl countElem M.empty
                  stream = (concatMap bigrams . concat) doc

{-| Looks up counts of a bigrams constituent words in a Map. Useful for the
    calculation of pMI -}
bigramWordsLookup :: NG T.Text -> CrackerMap -> (NG T.Text, Maybe Count, Maybe Count)
-- TODO: Possibly awkward to use tuple format. Possibly not. Evaluate.
-- TODO: Generalise this function to ngrams
bigramWordsLookup bg m = (bg, aC, bC) where
                    wrds = T.words $ M.fromJust $ getNG bg
                    aC = M.lookup a m
                    bC = M.lookup b m
                    a  = ngInject $ head wrds
                    b  = ngInject $ last wrds
                    -- the need for injections here suggest the possibility
                    -- of implementing a monad for this type.

{-| Looks up bigram's count in a Map. Useful for the calculation of pMI -}
bigramLookup :: NG T.Text -> CrackerMap -> (NG T.Text, Maybe Count)
bigramLookup bg m = (bg, count) where
-- m should be a Map of bigrams and their counts, not just words
                      count = M.lookup bg m

{-| Generalised version of bigramMap -}
ngramMap :: (SentColl T.Text -> SentColl T.Text) -> DocCol T.Text -> CrackerMap
ngramMap f doc = ngramMap' stream where
                   ngramMap' = foldl countElem M.empty
                   stream = (concatMap f . concat) doc

{-| Borrowed from: http://nlpwp.org/book/chap-words.xhtml. Word Count Map  -}
wcMap :: DocCol T.Text -> CrackerMap
wcMap doc = wcMap' stream where
              wcMap' = foldl countElem M.empty
              stream = concatMap concat doc

wcMapToList :: CrackerMap -> [(NG T.Text, Count)]
wcMapToList = M.toList

{-| Borrowed from: http://nlpwp.org/book/chap-words.xhtml. -}
countElem :: (Ord k) => M.Map k Count -> k -> M.Map k Count
countElem m e = case M.lookup e m of
                  Just v  -> M.insert e (v + 1) m
                  Nothing -> M.insert e 1 m

-- Notes on the why of Sets. Because of the way MI is calculated, the structure
-- of the document must be preserved; however, one still needs the unique
-- elements of the Document to to obtain counts for them


{- Makes a set from a list of words-}
wordSet :: [NG T.Text] -> CrackerSet
-- Haven't used type synonym because it's not necessarily a Sentence collection
wordSet = S.fromList

{- Makes a set from a docBody -}
wordSetDoc :: DocCol T.Text -> CrackerSet
wordSetDoc = S.fromList . concatMap concat

bigramSetDoc :: DocCol T.Text -> CrackerSet
bigramSetDoc = S.fromList . concatMap bigrams . concat

ngramSetDoc :: (SentColl T.Text -> SentColl T.Text) -> DocCol T.Text -> CrackerSet
ngramSetDoc ngf = S.fromList . concatMap ngf . concat

countWordSetElem :: CrackerSet -> DocCol T.Text -> [(NG T.Text, Count)]
countWordSetElem lexSet doc = countWordSetElem' lexSet concattedDoc where
                                concattedDoc = concatMap concat doc
                            
countWordSetElem' :: CrackerSet -> [NG T.Text] -> [(NG T.Text, Count)]
countWordSetElem' lexSet concattedDoc | S.null lexSet       = []
                                      | L.null concattedDoc = []
                                      | otherwise = (word, count) : 
                                          countWordSetElem' newSet newDoc where 
                                          word = S.findMin lexSet
                                          count = length $ filter (== word) concattedDoc
                                          newSet = S.deleteMin lexSet
                                          newDoc = filter (/= word) concattedDoc

test :: DocCol T.Text
test = [sents, sents', sents''] where
         sents  = map (map ngInject . T.words) [ "The quick brown fox jumped over the lazy dog"
                              , "Colourless green ideas sleep furiously"
                              , "Strange watching a VCA film not on purpose"
                              , "Heather Graham is bossy in this movie"
                              , "Each person has ridiculous boundary issues"
                              ]
         sents' =  map (map ngInject . T.words) [ "The quick brown fox jumped over the lazy dog"
                              , "They had very bad behavioural examples as children"
                              , "I can't stand commercials that are too loud"
                              , "There should be laws against that sort of thing"
                              ]

         sents'' = map (map ngInject . T.words) 
                                [ "The quick brown fox jumped over the lazy dog"
                                , "This film is very unrealistic"
                                , "Everyone radiates foolishness"
                                , "Corinne is horrifyingly mean to her children for no apparent reason"
                                , "It probably has to do with outragerously bad parental examples"
                                , "This film in one word sordid"
                                ]
