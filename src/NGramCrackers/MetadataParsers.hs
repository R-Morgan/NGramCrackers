{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module NGramCrackers.MetadataParsers 
( toMT
, tagParser
, mtParser
, metadataParser
, dateParser
, sDateParser
, toMonth
, toYear
, toDate
, toSDate
) where 

import Control.Applicative ((<$>), (<*), (*>), (<*>), (<|>), liftA3)
import Data.Functor (void)
import qualified Data.Text as T                    
import qualified Data.Either.Unwrap as EU (fromRight)
import qualified Text.Parsec.Text as PT 
import Text.ParserCombinators.Parsec hiding ((<|>))
import NGramCrackers.DataTypes
import NGramCrackers.ParagraphParsers (wordString)

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
           IndividualSub T.Text | Time Year | Region T.Text | Length Int
           deriving (Show, Read)

{- Date related data declarations -}

data TargetAudience = Adult | Children
data TargetGender = Female | Male | Trans | NonBinary | Genderfluid | GenderQ

data Biography = Yes | No | Autobiography
{- Metadata parsing function -}

tagParser :: PT.Parser T.Text
tagParser = (between left right $ parser) <* many space'
              where left   = (char '<')
                    right  = (char '>') 
                    parser = T.pack <$> (many1 letter)
                    space' = char ' '

-- TODO contentsParser needs to be able to handle parsing multiword sequences 
-- and dates, not just strings of letters and numbers
contentsParser :: PT.Parser T.Text
contentsParser = T.pack <$> ((letts <|> nums) <* (many space'))
                   where letts  = (many1 letter)
                         nums   = (many1 digit)
                         space' = char ' '

mtParser :: PT.Parser MetaTag
mtParser = toMT <$> tagParser <*> contentsParser

-- TODO -- Pattern matching Record-based handling of parsed data

handler :: MetaTag -> Tag
handler MetaTag{..} = case tag of
                        "SDF" -> SDF int
                        "REF" -> FileName contents
                        "TIT" -> Title contents
                        "ENT" -> Entry date                         
                        "TIM" -> Time year
                        "EXL" -> Length int
                        "SUP" -> SuperField int
                        "SUB" -> Subject contents
--                        "LEV" -> Level contents

                        _     -> error "Invalid tag"
                        where int  = ((read . T.unpack) contents) :: Int
                                --   T.Text ->  String -> Int
                              date = EU.fromRight $ (parse dateParser "unknown" contents)
                              year = (Year int)
                                -- this fromRight bit seems a bit...amateurish
                              string = EU.fromRight $ (parse wordString "unknown" contents)
 
--entryParser :: PT.Parser Tag
--entryParser = toEntry <$> tagParser <*> dateParser contentsParser


metadataParser :: PT.Parser [MetaTag]
metadataParser = sepBy mtParser newLn 
                   where newLn = (char '\n')

dateParser :: PT.Parser Date
dateParser = liftA3 toDate month day year
               where month = dP toMonth <$> (many1 digit) <* seppr
                     day   = dP toDay   <$> (many1 digit) <* seppr
                     year  = dP toYear  <$> (many1 digit)
                     seppr = (char '/')
                     dP f  = f . read

sDateParser :: PT.Parser SDate
sDateParser = toSDate <$> month <*> day
                where month = dP toMonth <$> (many1 digit) <* seppr 
                      day   = dP toDay   <$> (many1 digit)
                      seppr = (char '/')
                      dP f  = f . read

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

toEntry :: Date -> Tag
toEntry = Entry
