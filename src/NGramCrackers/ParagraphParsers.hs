{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.ParagraphParsers (
  parseSent
, parseParagraph
--, parseMultiPara
) where


import Control.Applicative ((<$>), (<*), (*>), (<*>), (<|>), liftA3)
import Data.Functor (void)
import Data.List (concat, unwords)
import Data.Text as T                    
import Text.Parsec.Text as PT 
import Text.ParserCombinators.Parsec hiding ((<|>))
import NGramCrackers.DataTypes

parseSent :: T.Text -> Either ParseError [T.Text]
parseSent = parse sentence "unknown"

parseParagraph :: T.Text -> Either ParseError [[T.Text]]
parseParagraph = parse paragraph "unknown"

parseMultiPara :: T.Text ->  Either ParseError [[[T.Text]]]
parseMultiPara = parse document "unknown"

document :: PT.Parser [[[T.Text]]]
document = endBy paragraph eop

paragraph :: PT.Parser [[T.Text]]
paragraph = endBy sentence eos

sentence :: PT.Parser [T.Text]
sentence = sepBy word seppr

word :: PT.Parser T.Text
-- The use of T.pack <$> is necessary because of the type many1 letter returns.
-- fmapping T.pack into the Parser makes it possible to return a parser of the
-- appropriate type.
word = T.pack <$> ((many1 letter) <|> (many1 digit))
                                                     
seppr :: PT.Parser ()
-- Since the results of this parser are just thrown away, we need the `void`
-- function from Data.Functor
seppr =  void sepprs <|> void newLn
           where sepprs =    space'
                         <|> (char ',' *> space')
                         <|> (char ';' *> space')
                         <|> (char ':' *> space')
                 newLn  =    ((many1 (char '\n')))
                 space' = char ' '

eos :: PT.Parser ()
eos = void sepprs -- <|> void sngls
        where sepprs =    (char '.' *> space')
                      <|> (char '!' *> space')
                      <|> (char '?' *> space')
{-              sngls  =    (char '.')
                      <|> (char '!')
                      <|> (char '?')
-}
              space' =    char ' '

eop :: PT.Parser ()
eop = void $ 
  char '<' >> many1 letter >> char '>' *> (void space' <|> void newLn)
    where space' = char ' '
          newLn  = (many1 (char '\n'))

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

{- Elementary parser combinataors. -}


{-  It might be useful to get this rolling for a more flexible eos
eos       =    try (string ". ")
           <|> try (string "! ") 
           <|> try (string "? ")
           <?> "end of sentence"

eop :: Parser String
eop = string "<para>"

eos :: Parser Char
eos       = oneOf ".?!" -- end of sentence

{-| -}
parseParagraph :: String -> Either ParseError [[String]]
parseParagraph = parse paragraph "unknown" 

parseMultiPara :: String ->  Either ParseError [[[String]]]
parseMultiPara = parse document "unknown" 

-}
 
