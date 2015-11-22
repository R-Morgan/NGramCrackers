{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.Parsers.Paragraph
( parseSent
, parseParagraph
, parseMultiPara
, wordString
) where

import Data.Text as T
import Text.Parsec.Text as PT

import Control.Applicative ((<$>), (<*), (*>), (<*>), (<|>), liftA3)
import Data.Functor (void)
import Data.List (concat, unwords)
import Text.ParserCombinators.Parsec hiding ((<|>))

import NGramCrackers.DataTypes
import NGramCrackers.Ops.Retyped

parseMultiPara :: T.Text ->  Either ParseError (DocCol T.Text)
parseMultiPara = parse docBody "unknown"

parseParagraph :: T.Text -> Either ParseError (ParaColl T.Text)
parseParagraph = parse paragraph "unknown"

parseSent :: T.Text -> Either ParseError (SentColl T.Text)
parseSent = parse sentence "unknown"

docMetadata :: [MetaTag]
docMetadata = undefined

docBody :: PT.Parser (DocCol T.Text)
docBody = endBy paragraph eop

paragraph :: PT.Parser (ParaColl T.Text)
paragraph = endBy sentence eos

sentence :: PT.Parser (SentColl T.Text)
sentence = sepBy sentParts seppr

sentParts :: PT.Parser (NGram T.Text)
sentParts = ngram <|> numToNG

wordString :: PT.Parser T.Text
-- Useful for non-sentence word strings where no numbers need to be parsed.
-- Probably useful for parsing MetaTags
wordString = T.unwords <$> sepBy word seppr

ngramSeries :: PT.Parser (SentColl T.Text)
ngramSeries = sepBy ngram seppr

ngram :: PT.Parser (NGram T.Text)
ngram = (ngramInject) <$> word

word :: PT.Parser T.Text
-- The use of T.pack <$> is necessary because of the type many1 letter returns.
-- fmapping T.pack into the Parser makes it possible to return a parser of the
-- appropriate type.
word = T.pack <$> many1 letter 

numToNG :: PT.Parser (NGram T.Text)
numToNG = fmap ngramInject number

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
