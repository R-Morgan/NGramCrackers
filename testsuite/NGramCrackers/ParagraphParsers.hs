module NGramCrackers.ParagraphParsers (
  parseParagraph
, flattenEither
) where

import Text.ParserCombinators.Parsec
import Data.Either.Unwrap (mapRight)
import Data.List (concat, unwords)

{- Elementary parser combinataors. -}

--paragraphFile = do 
                  
paragraph = sepBy sentence eos

sentence  = sepBy word seppr -- (oneOf " \n") 

word      = many (noneOf " .?!\n") 

--sentence  = sepBy word seppr

seppr     =    try space 
           <|> try (char '\n')
           <?> "word separator"


{- 
eos       =    try (string ". ")
           <|> try (string "! ") 
           <|> try (string "? ")
           <?> "end of sentence"
-}

eos       = oneOf ".?!"

{-| -}
parseParagraph :: String -> Either ParseError [[String]]
parseParagraph = parse paragraph "unknown" 

--parseParagraphFile :: String -> Either ParseError [[String]]
--parseParagraphFile input = parse paragraphFile "(unknown)" input 

{-| -}
flattenEither :: Either a [[b]] -> Either a [b]
flattenEither = mapRight concat

-- flattenEither e _  = 

{- 
csvFile = endBy line eol
line = sepBy cell (char ' ')
cell = many (noneOf " \n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
-}
