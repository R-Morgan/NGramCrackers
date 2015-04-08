module NGramCrackers.Quant.Counts
( bigramMap
, ngramMap
, wcMap
, countElem
, wordSet
, wordSetDoc
, ngramSetDoc
, countWordSetElem
) where

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Data.Set as S

import NGramCrackers.Ops.Text
import NGramCrackers.Utilities.List
import NGramCrackers.Parsers.Paragraph
import NGramCrackers.Quant.Dispersion

{-| Produces Map of bigrams in a document-}
bigramMap :: [[[T.Text]]] -> M.Map T.Text Int
bigramMap doc = bigramMap' stream where
                  bigramMap' = foldl countElem M.empty
                  stream = (concatMap bigrams . map T.unwords . concat) doc

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
countElem m e = case (M.lookup e m) of
                  Just v  -> M.insert e (v + 1) m
                  Nothing -> M.insert e 1 m

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


