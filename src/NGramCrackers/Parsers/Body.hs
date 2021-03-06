{-# LANGUAGE OverloadedStrings #-}

module NGramCrackers.Parsers.Body
(
  parseSent
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
import NGramCrackers.Ops.NG

parseMultiPara :: T.Text ->  Either ParseError (DocCol T.Text)
parseMultiPara = parse docBody "unknown"

parseParagraph :: T.Text -> Either ParseError (ParaColl T.Text)
parseParagraph = parse paragraph "unknown"

parseSent :: T.Text -> Either ParseError (SentColl T.Text)
parseSent = parse sentence "unknown"

docMetadata :: [MetaTag]
-- May need to be moved to the Metadata module
docMetadata = undefined

docBody :: PT.Parser (DocCol T.Text)
docBody = endBy paragraph eop

paragraph :: PT.Parser (ParaColl T.Text)
paragraph = endBy sentence eos

sentence :: PT.Parser (SentColl T.Text)
sentence = sepBy sentParts seppr

sentParts :: PT.Parser (NG T.Text)
sentParts = ngram <|> numToNG

ngramSeries :: PT.Parser (SentColl T.Text)
ngramSeries = sepBy ngram seppr

numToNG :: PT.Parser (NG T.Text)
numToNG = fmap ngInject number where number = T.pack <$> many1 digit

{-| This parser is the basis for most all the parsers above. The parser puts
    a parsed word into the NG record context. -}
ngram :: PT.Parser (NG T.Text)
ngram = ngInject <$> word where word = T.pack <$> many1 letter

wordString :: PT.Parser T.Text
-- Useful for non-sentence word strings where no numbers need to be parsed.
--  Probably useful for parsing MetaTags
wordString = T.unwords <$> sepBy word seppr where word = T.pack <$> many1 letter
-- The use of T.pack <$> is necessary because of the type many1 letter returns.
-- fmapping T.pack into the Parser makes it possible to return a parser of the
-- appropriate type.

--------------------------------------------------------------------------------
-- Separation parsers, used to parse and discard punctuation
--------------------------------------------------------------------------------

{-| For parsing common within sentence separators.-}
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
{-| Intended to parse separators that demarcate the ends of sentences, when
 -  followed by spaces. Unsure why I was not able to get '. ' and '.' to parse
 -  as end of sentence markers. Possible solutioon may be to make separate
 -  parsers and conjoin them in a new one a la sentParts. -}
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
{-| Intended to parse the '<para>' sequence so common in the SGML documents,
    when followed by a a space or newline. Parser may be useful guide for
    handling the metadata tags. -}
eop :: PT.Parser ()
eop = void $ 
  char '<' >> many1 letter >> char '>' <* (void space' <|> void newLn)
    where space' = char ' '
          newLn  = many1 (char '\n')
