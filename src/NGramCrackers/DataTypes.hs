{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.DataTypes
( ParaColl (..)
, SentColl (..)
, NGram (..)
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


-------------------------------------------------------------------------------
-- Collection of ngrams in a paragraph
--
data ParaColl a = ParaColl [SentColl a] deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
-- Collection of ngrams in a sentence
data SentColl a = SentColl [NGram a] deriving (Show, Read, Eq)

-------------------------------------------------------------------------------
--NGram Type
data NGram a = Wrd a | Bigram a | Trigram a | NGram Int a deriving (Show, Read, Eq)
  -- Int represents the length of the ngram in words

---- Instance declarations
instance Functor (NGram) where
    --fmap :: (a -> b) -> f a -> f b
    fmap f (Wrd txt) = Wrd (f txt)
    fmap f (Bigram txt) = Bigram (f txt)
    fmap f (Trigram txt) = Trigram (f txt)
    fmap f (NGram n txt) = NGram n (f txt)

ngramInject :: T.Text -> NGram T.Text
ngramInject txt | phraseLen < 1 = error "Not an n-gram"
                | phraseLen == 1 = Wrd txt
                | phraseLen == 2 = Bigram txt
                | phraseLen == 3 = Trigram txt
                | phraseLen  < 8 = NGram phraseLen txt
                | otherwise = error "Phrase too large" where
                       phraseLen = (length . T.words) txt

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
