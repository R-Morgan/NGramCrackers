{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.DataTypes
( Paragraph
, Sentence
, Word
, PageRange (..)
, PageBound (..)
, Level     (..)
) where

import qualified Data.Text as T

data Paragraph = Paragraph [Sentence] deriving (Show, Read, Eq)
data Sentence = Sentence [Word] deriving (Show, Read, Eq)
type Word = T.Text


-- Page Range
data PageRange =  PageRange PageBound PageBound  deriving (Show, Read, Eq)
data PageBound = Start Int | End Int deriving (Show, Read, Eq)

-- Difficult y Level
data Level = LitFic | PopFic | Tech | Lay | PopNonFic | Other

toLevel :: T.Text -> Level
toLevel text | text == "litf" = LitFic
             | text == "popf" = PopFic
             | text == "tech" = Tech
             | text == "lay"  = Lay
             | text == "popn" = PopNonFic
             | otherwise = error "Non-recognised level"
