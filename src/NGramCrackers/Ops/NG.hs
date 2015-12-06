module NGramCrackers.Ops.NG
(
  bigrams
, trigrams
, getNGramsFromText
, getTrueNGrams
, countWords
) where

import qualified Data.Text as T

import Data.Char (isAlpha, isAlphaNum, isSpace, toLower)
import Data.List (map, nub)
import Data.Monoid ((<>))

import NGramCrackers.DataTypes
import NGramCrackers.Ops.Infixes
import NGramCrackers.Utilities.List

{-| Extract bigrams from a list of NGs -}
bigrams :: SentColl T.Text -> SentColl T.Text
bigrams = getTrueNGrams 2

{-| Extract trigrams from a list of NGs -}
trigrams :: SentColl T.Text -> SentColl T.Text
trigrams = getTrueNGrams 3

{-| Extract n-grams from a string -}
getNGramsFromText :: Int -> T.Text -> SentColl T.Text
getNGramsFromText n packed -- packed is a packed T.Text string
    | n < 0     = error "n must be a positive integer less than 7"
    | n > 7     = error "n must be a positive integer less than 7"
    | otherwise = getTrueNGrams n wordList
                    where wordList = map ngInject $ T.words packed

getTrueNGrams :: Int -> [NG T.Text] -> [NG T.Text]
getTrueNGrams n [] = []
getTrueNGrams n list@(x:xs) 
    | n < 0     = error "n must be a positive integer less than 7"
    | n > 7     = error "n must be a positive integer less than 7"
    | length list >= n = ng : getTrueNGrams n xs 
    | otherwise = [] where
      ng = Prelude.foldr (<>) NG{ getNG = Nothing, len = 0} phrase
      phrase = take n list

countWords :: T.Text -> Int
-- Cheap hack that  should be done away with in the main options area
-- This is not a good place for the function, but it will do temporarily
countWords =  length . T.words

