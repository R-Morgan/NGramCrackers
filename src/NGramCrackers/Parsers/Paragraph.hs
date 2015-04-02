{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.Parsers.Paragraph
( parseSent
, parseParagraph
, parseMultiPara
, wordString
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
parseMultiPara = parse docBody "unknown"

docMetadata :: [MetaTag]
docMetadata = undefined

docBody :: PT.Parser [[[T.Text]]]
docBody = endBy paragraph eop

paragraph :: PT.Parser [[T.Text]]
paragraph = endBy sentence eos

sentence :: PT.Parser [T.Text]
sentence = sepBy sentParts seppr

sentParts :: PT.Parser T.Text
sentParts = word <|> number

wordString :: PT.Parser T.Text
-- Useful for non-sentence word strings where no numbers need to be parsed.
-- Probably useful for parsing MetaTags
wordString = T.unwords <$> sepBy word seppr

word :: PT.Parser T.Text
-- The use of T.pack <$> is necessary because of the type many1 letter returns.
-- fmapping T.pack into the Parser makes it possible to return a parser of the
-- appropriate type.
word = T.pack <$> many1 letter 

number :: PT.Parser T.Text
number = T.pack <$> many1 digit
                                                     
seppr :: PT.Parser ()
-- Since the results of this parser are just thrown away, we need the `void`
-- function from Data.Functor
seppr =  void sepprs <|> void newLn
           where sepprs =    space'
                         <|> (char ',' *> space')
                         <|> (char ';' *> space')
                         <|> (char ':' *> space')
                 newLn  =    many1 (char '\n')
                 space' = char ' '

eos :: PT.Parser ()
eos = void sepprs -- <|> void sngls
        where sepprs =    (char '.' <* space')
                      <|> (char '!' <* space')
                      <|> (char '?' <* space')
{-              sngls  =    (char '.')
                      <|> (char '!')
                      <|> (char '?')
-}
              space' = many (char ' ')

eop :: PT.Parser ()
eop = void $ 
  char '<' >> many1 letter >> char '>' <* (void space' <|> void newLn)
    where space' = char ' '
          newLn  = many1 (char '\n')
