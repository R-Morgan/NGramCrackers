{-# LANGUAGE OverloadedStrings
           , FlexibleInstances #-}

module NGramCrackers.DataTypes
( DocCol    (..)
, ParaColl  (..)
, SentColl  (..)
, NGram     (..)
, MetaTag   (..)
, Date      (..)
, SDate     (..)
, Month     (..) 
, Day       (..)
, Year      (..)
, Tag       (..)
, Medium    (..)
, PageRange (..)
, PageBound (..)
, Level     (..)
, toMedium
) where

import qualified Data.Text as T
import NGramCrackers.Ops.Text ((<#>))

-------------------------------------------------------------------------------
dtester :: DocCol T.Text
dtester = [ [tester, tester, tester]
          , [tester, tester]
          ]

ptester :: ParaColl T.Text
ptester = [tester, tester]

tester :: SentColl T.Text
tester = ngrams where
           ngrams = map ngramInject ["foo", "bar", "baz", "shay"]


-------------------------------------------------------------------------------
type DocCol a = [ParaColl a]

-------------------------------------------------------------------------------
-- Collection of ngrams in a paragraph
--
--data ParaColl a = NullPara | ParaColl (SentColSeq a) deriving (Show, Read, Eq)
-- Instance declarations
     -- Functor
     -- Monoid
     -- Applicative?
     -- Monad?

--instance Functor (ParaColl) where
--    fmap :: (a -> b) -> f a -> f b
    --fmap f NullPara             = NullPara
--    fmap f (ParaColl ngrams) = ParaColl (f ngrams)
    --fmap f (ParaColl sentColls) = ParaColl ((map (fmap f)) sentColls)
    -- Similar issue here as with the fmap implementation in SentColl

type ParaColl a = [SentColl a]
-------------------------------------------------------------------------------
-- Collection of ngrams in a sentence
--data SentColl a = NullSent | SentColl (NGSeq a) deriving (Show, Read, Eq)

type SentColl a = [(NGram a)]
-- Instance declarations
     -- Monoid
     -- Applicative?
     -- Monad?
--type SentColSeq a = [(SentColl a)]
-- Type synonym for a list of NGram a

--instance Functor (SentColl) where
--    fmap :: (a -> b) -> f a -> f b
--    fmap f NullSent          = NullSent
    --fmap f (SentColl ngrams) = SentColl (f ngrams)
--    fmap f (SentColl ngrams) = SentColl ((map (fmap f)) ngrams)
    -- This implementation of fmap pipes the function to be done on the
    -- NGram level. I reckon this will make it difficult to do something like
    -- get the length of the list of NGram. This is a useful function, but I
    -- think it goes too deeply into the container structure to be the proper
    -- implementation of fmap for this type.

--instance Monoid (SentColl T.Text) where
-- Is this really best described as Monoid? Problem with mappend is that
-- to do that to  SentCollS merges two sentence collections into one SentColl
-- container. In reality, concatentation of two SentColls should be really be
-- in a list -- i.e. a ParaColl.
    --mempty  = NullSent

    --mappend (SentColl ngrams) NullSent = (SentColl ngrams)
    --mappend NullSent (SentColl ngrams) = (SentColl ngrams)
    --mappend (SentColl ngrams) (SentColl ngrams') = (SentColl $ ngrams ++ ngrams')

--    mconcat = undefined

-------------------------------------------------------------------------------
-- NGram Type
data NGram a =  NullGram
               -- | Wrd a
               -- | Bigram a
               -- | Trigram a
              | NGram Int a deriving (Show, Read, Eq, Ord)
  -- Int represents the length of the ngram in words
  -- Is this type too flexible? NGrams should really only be T.Text

--type NGSeq a = [(NGram)]
-- Type synonym for a list of NGram a

-- Instance declarations
     -- Applicative?
     -- Monad?

instance Functor (NGram) where
    --fmap :: (a -> b) -> f a -> f b
    --fmap f (Wrd txt)     = Wrd (f txt)
    --fmap f (Bigram txt)  = Bigram (f txt)
    --fmap f (Trigram txt) = Trigram (f txt)
    fmap f NullGram      = NullGram
    fmap f (NGram n txt) = NGram n (f txt)

instance Monoid (NGram T.Text) where
    mempty  = NullGram

    mappend (NGram n txt) NullGram       = (NGram n txt)
    mappend NullGram (NGram n txt)       = (NGram n txt)
    -- Turns out that you can't use mempty in place of NullGram in the identity
    -- parts of the mappend definition
    mappend (NGram n txt) (NGram m txt') = (NGram (n + m) (txt <#> " " <#> txt'))

    mconcat = undefined

ngramInject :: T.Text -> NGram T.Text
ngramInject txt | phraseLen < 1 = error "Not an n-gram"
--                | phraseLen == 1 = Wrd txt
--                | phraseLen == 2 = Bigram txt
--                | phraseLen == 3 = Trigram txt
                | phraseLen  < 8 = NGram phraseLen txt
                | otherwise = error "Phrase too large" where
                       phraseLen = (length . T.words) txt

-------------------------------------------------------------------------------

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

-- Difficulty Level
data Level = LitFic | PopFic | Tech | Lay | PopNonFic | OtherLvl T.Text

toLevel :: T.Text -> Level
toLevel text | text == "litf" = LitFic
             | text == "popf" = PopFic
             | text == "tech" = Tech
             | text == "lay"  = Lay
             | text == "popn" = PopNonFic
             | otherwise = OtherLvl text
