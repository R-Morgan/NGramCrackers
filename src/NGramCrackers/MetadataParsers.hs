module NGramCrackers.MetadataParsers 
( toMT
, toMonth
, toDay
, toYear
, toDate
, toSDate
) where 

{- Metadata parsing function -}

tagParser :: PT.Parser T.Text
tagParser = (between left right $ parser) <* many space'
              where left   = (char '<')
                    right  = (char '>') 
                    parser = pack <$> (many1 letter)
                    space' = char ' '

contentsParser :: PT.Parser T.Text
contentsParser = pack <$> ((letts  <|> nums) <* (many space'))
                   where letts  = (many1 letter)
                         nums   = (many1 digit)
                         space' = char ' '

mtParser :: PT.Parser MetaTag
mtParser = toMT <$> tagParser <*> contentsParser

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
