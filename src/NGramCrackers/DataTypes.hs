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
, toMT
, toMonth
, toDay
, toYear
, toDate
, toSDate
--, dateFromSDate
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

data SDate = SDate Month Day 
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
toMonth n | n == 1 = Jan
          | n == 2 = Feb
          | n == 3 = March
          | n == 4 = April
          | n == 5 = May
          | n == 6 = June
          | n == 7 = July
          | n == 8 = Aug
          | n == 9 = Sept
          | n == 10 = Oct
          | n == 11 = Nov
          | n == 12 = Dec
          | otherwise = error "Not a month"

toYear :: Int -> Year
toYear = Year 

toDay :: Int -> Day
toDay n | n < 1  = error "Not a day!"
        | n > 31 = error "Not a day!"
        | otherwise = Day n

toDate :: Month -> Day -> Year -> Date
toDate m d y = Date m d y

--dateFromSDate :: SDate -> Year -> Date
--dateFromSDate (SDate sd) y  = Date sd y

toSDate :: Month -> Day -> SDate
toSDate = SDate 
