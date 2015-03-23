module NGramCrackers.DataTypes
( Paragraph
, Sentence
, Word
, MetaTag
, Tag
, Date
, SDate
, Month
, Day
, Year
, PageRange
, PageBound
, Level
) where

import qualified Data.Text as T

data Paragraph = Paragraph [Sentence] deriving (Show, Read, Eq)
data Sentence = Sentence [Word] deriving (Show, Read, Eq)
type Word = T.Text


