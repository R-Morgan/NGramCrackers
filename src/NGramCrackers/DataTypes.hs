{-# LANGUAGE  OverloadedStrings
            , FlexibleInstances #-}

module NGramCrackers.DataTypes
( DocCol     (..)
, ParaColl   (..)
, SentColl   (..)
, Count      (..)
, CrackerMap (..)
, CrackerSet (..)
, NG         (..)
, MetaTag    (..)
, Date       (..)
, SDate      (..)
, Month      (..)
, Day        (..)
, Year       (..)
, Tag        (..)
, Medium     (..)
, PageRange  (..)
, PageBound  (..)
, Level      (..)
, toMedium
, ngInject
, dtester
, ptester
, tester
) where

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T

import NGramCrackers.Ops.Infixes ((<#>))
-- Could this infix be replaced with mappend?

-------------------------------------------------------------------------------
dtester :: DocCol T.Text
dtester = [ [tester, tester, tester]
          , [tester, tester]
          ]

ptester :: ParaColl T.Text
ptester = [tester, tester]

tester :: SentColl T.Text
tester = ngrams where
           ngrams = map ngInject ["foo", "bar", "baz", "shay"]


-------------------------------------------------------------------------------
-- Type Synonyms for health and happiness

type DocCol a   = [ParaColl a]
type ParaColl a = [SentColl a]
type SentColl a = [NG a]

type Count = Int -- Useful for modules dealing with counts of phrasal structures
type MI    = Double -- (pointwise) mutual information score

type CrackerMap = M.Map (NG T.Text) Count
type CrackerSet = S.Set (NG T.Text)
-------------------------------------------------------------------------------

-- Instance declarations
     -- Applicative?
     -- Monad?
     -- Special Show instance?
     -- Other?

data NG a = NG { getNG :: Maybe a
               , len   :: Int     } deriving (Show, Read, Eq, Ord)

instance Functor NG where
    fmap f NG{ getNG  = Nothing, len = 0 }      = NG { getNG = Nothing, len = 0 }
    fmap f NG{ getNG  = Just m , len   = x }   = NG { getNG = Just $ f m , len   = x }

instance Monoid (NG T.Text) where
    mempty  = NG { getNG = Nothing , len   = 0 }

    mappend NG { getNG = Nothing , len = 0} NG{ getNG = Just txt, len = n} =
      NG{ getNG = Just txt, len = n }

    mappend NG{ getNG = Just txt, len = n} NG{ getNG = Nothing , len = 0} =
      NG{ getNG = Just txt, len = n }
    -- Turns out that you can't use mempty in place of NullGram in the identity
    -- parts of the mappend definition
    --
    mappend NG{ getNG = Just txt, len = n } NG { getNG = Just txt', len = m } =
      NG { getNG = Just $ txt <#> " " <#> txt', len = n + m }
    -- mappend allows for the concatenation of the ngrams, while also adding
    -- their lengths together. Monoids are pretty slick.

ngInject :: T.Text -> NG T.Text
ngInject ("") = NG { getNG = Nothing,  len   = 0}
ngInject txt  = NG { getNG = Just txt, len   = (length . T.words) txt }

-------------------------------------------------------------------------------
-- Metadata data declarations

data MetaTag = MetaTag { tag      :: T.Text
                       , contents :: T.Text
                       } deriving (Show, Read)

{- Date related data declarations -}
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
           IndividualSub T.Text | Med Medium | Time Year | Region T.Text | 
           Length Int deriving (Show, Read)

data Medium = Book | Journal | Newspaper | OtherMed T.Text deriving (Read, Show)

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
