module NGramCrackers.Ops.Retyped
(
  bigrams
, trigrams
, getNGramsFromText
, getTrueNGrams
, getNGramsFromTextList
, getAlphasOnlyToText
, normToList
, getWordFrequency
, countWords
, typeTokenRatio
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
      ng = Prelude.foldr ((<>)) NG{ getNG = Nothing, len = 0} phrase
      phrase = take n list

getNGramsFromTextList :: Int -> [T.Text] -> [[T.Text]]
getNGramsFromTextList = getNSeqFromList

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnlyToText :: T.Text -> T.Text
getAlphasOnlyToText = T.filter (\char -> isAlpha char || isSpace char)

{-| Return only alphanumeric characters from a string and return the
    result as a List.-}
normToList :: T.Text -> [T.Text]
normToList = T.words . getAlphasOnlyToText . T.toLower

{-| Get frequency of a single word's occurance in a string. Is eta-reduction
    the easiest reading way to do this function? The arguments are fairly
    instructive. However, the type declaration does say what kind of args
    it takes.  With type synonyms or further exploring the type system,
    the declaration would be more informative-}

getWordFrequency:: T.Text -> T.Text -> Int
-- No eta-reduction for readability
getWordFrequency word text = (length . filter (== word) . T.words) text

{-| These functions output the specified strings, so they can be kept and
 developed separately from the lists that get used in generating the
 the help, version, and about type displays -}

countWords :: T.Text -> Int
countWords =  length . T.words

typeTokenRatio ::  T.Text -> T.Text 
typeTokenRatio string = typeStr <#> ps types <#> tokStr <#> ps tokens <#> ttrStr 
                                <#> ratio  
                       where types = (fromIntegral . length . nub . T.words) string
                             tokens = (fromIntegral . length . T.words) string
                             ratio = (T.pack . show) $ types / tokens
                             typeStr = T.pack "Types: "  
                             tokStr  = T.pack ", Tokens: "  
                             ttrStr  = T.pack ", TTR: "
                             ps      = T.pack . show
