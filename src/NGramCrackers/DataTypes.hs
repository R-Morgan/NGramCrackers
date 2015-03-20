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
) where

import qualified Data.Text as T

data Paragraph = Paragraph [Sentence] deriving (Show, Read, Eq)
data Sentence = Sentence [Word] deriving (Show, Read, Eq) 
type Word = T.Text 

data MetaTag = MetaTag { tag      :: T.Text,
                         contents :: T.Text
                       } deriving (Show)

data Tag = SDF Int | Reference T.Text | Entry Date | Pages PageRange
           deriving (Show, Read)

{- mkTag :: T.Text -> Tag
mkTag txt int = case txt of
                  "SDF" -> SDF int
                  "REF" -> Reference
-}


{- Date related data declarations -}

data Date = Date Month Day Year
              deriving (Show, Read, Eq)

data SDate = SDate Month Day 
              deriving (Show, Read, Eq)

data Month = Jan | Feb | March | April | May | June | July | Aug | 
             Sept | Oct | Nov | Dec deriving (Show, Read, Eq, Enum)

data Day = Day Int deriving (Show, Read, Eq)

data Year = Year Int deriving (Show, Read, Eq)

-- Page Range
data PageRange =  PageRange PageBound PageBound  deriving (Show, Read, Eq)
data PageBound = Start Int | End Int deriving (Show, Read, Eq)


