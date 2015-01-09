module NGramCrackers.ParagraphParsers (
  parseParagraph
, parseMultiPara
, flattenEither
) where

import Text.ParserCombinators.Parsec
import Data.Either.Unwrap (mapRight)
import Data.List (concat, unwords)

{- Elementary parser combinataors. -}

document = sepBy paragraph eop

paragraph = sepBy sentence eos

sentence  = sepBy word seppr -- (oneOf " \n") 

word      = many letter

seppr     =    try space 
           <|> try (char ',')
           <|> try (char '\n')
           <?> "word separator"


{- 
eos       =    try (string ". ")
           <|> try (string "! ") 
           <|> try (string "? ")
           <?> "end of sentence"
-}

eop = string "<para>"

eos       = oneOf ".?!" -- end of sentence


{-| -}
parseParagraph :: String -> Either ParseError [[String]]
parseParagraph = parse paragraph "unknown" 

parseMultiPara :: String ->  Either ParseError [[[String]]]
parseMultiPara = parse document "unknown"

{-| -}
flattenEither :: Either a [[b]] -> Either a [b]
flattenEither = mapRight concat

