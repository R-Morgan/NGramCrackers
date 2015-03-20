module NGramCrackers.MetadataParsers 
( toMT
, toMonth
, toDay
, toYear
, toDate
, toSDate
) where 

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
