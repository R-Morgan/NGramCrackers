module NGramCrackers.DataTypes
( Paragraph
, Sentence
, Word
, MetaTag
, Tag
, Date
, Month
, Day
, toMT
, toMonth
, toDay
, toYear
) where

import qualified Data.Text as T

data Paragraph = Paragraph [Sentence] deriving (Show, Read, Eq)
data Sentence = Sentence [Word] deriving (Show, Read, Eq) 
type Word = T.Text 

data MetaTag = MetaTag { tag      :: T.Text,
                         contents :: T.Text
                       } deriving (Show)

data Tag = SDF Int | Reference T.Text -- | Entry Date | Pages PageRange
           deriving (Show, Read)

{- mkTag :: T.Text -> Tag
mkTag txt int = case txt of
                  "SDF" -> SDF int
                  "REF" -> Reference
-}


data Date = Date Month Day Year
              deriving (Show, Read, Eq)

data Month = Jan | Feb | March | April | May | June | July | Aug | Sept | Oct |
             Nov | Dec deriving (Show, Read, Eq, Enum)

data Day = Day Int deriving (Show, Read, Eq)

data Year = Year Int deriving (Show, Read, Eq)

--data DayCount  = 1 | 2 | ... | 31 
--data PageRange

toMT :: T.Text -> T.Text -> MetaTag
toMT tag' val = MetaTag { tag = tag', contents = val}
                       
toMonth :: Int -> Month
toMonth | 1 = Jan
        | 2 = Feb
        | 3 = March
        | 4 = April
        | 5 = May
        | 6 = June
        | 7 = July
        | 8 = Aug
        | 9 = Sept
        | 10 = Oct
        | 11 = Nov
        | 12 = Dec

toYear :: Int -> Year
toYear = Year 

toDay :: Int -> Day
toDay = Day


