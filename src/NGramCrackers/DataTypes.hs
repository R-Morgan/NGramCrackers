{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.DataTypes
( Paragraph
, Sentence
, Word
, MetaTag (..)
, Date (..)
, SDate (..)
, Month (..) 
, Day (..)
, Year (..)
, Tag (..)
, PageRange (..)
, PageBound (..)
, Level     (..)
, Medium (..)
, toMedium
) where

import qualified Data.Text as T

data Paragraph = Paragraph [Sentence] deriving (Show, Read, Eq)
data Sentence = Sentence [Word] deriving (Show, Read, Eq)
type Word = T.Text

data MetaTag = MetaTag { tag      :: T.Text
                       , contents :: T.Text
                       } deriving (Show, Read)

data Date = Date Month Day Year
              deriving (Show, Read, Eq)

data SDate = SDate Month Day 
              deriving (Show, Read, Eq)

data Month = Jan | Feb | March | April | May | June | July | Aug |
             Sept | Oct | Nov | Dec deriving (Show, Read, Eq, Enum)

data Day = Day Int deriving (Show, Read, Eq)

data Year = Year Int deriving (Show, Read, Eq)

data Tag = SDF Int | FileName T.Text | Entry Date | Pages PageRange |
           Title T.Text | Publication T.Text Year | FstEdition T.Text Year |
           Notes T.Text | Authors [T.Text] | Gender [T.Text] |
           Race [T.Text] | SuperField Int | Subject T.Text |
           IndividualSub T.Text | Med Medium | Time Year | Region T.Text | Length Int
           deriving (Show, Read)

data Medium = Book | Journal | Newspaper | OtherMed T.Text deriving (Read, Show)
{- Date related data declarations -}

toMedium :: T.Text -> Medium
toMedium text | text == "book"    = Book
              | text == "journal" = Journal
              | text == "newsp"   = Newspaper
              | otherwise = OtherMed text

data TargetAudience = Adult | Children
data TargetGender = Female | Male | Trans | NonBinary | Genderfluid | GenderQ

data Biography = Yes | No | Autobiography

-- Page Range
data PageRange =  PageRange PageBound PageBound  deriving (Show, Read, Eq)
data PageBound = Start Int | End Int deriving (Show, Read, Eq)

-- Difficult y Level
data Level = LitFic | PopFic | Tech | Lay | PopNonFic | OtherLvl T.Text

toLevel :: T.Text -> Level
toLevel text | text == "litf" = LitFic
             | text == "popf" = PopFic
             | text == "tech" = Tech
             | text == "lay"  = Lay
             | text == "popn" = PopNonFic
             | otherwise = OtherLvl text
