module NGramCrackers.ParagraphParsers (
  parseParagraph
, parseMultiPara
) where

import Text.ParserCombinators.Parsec
import Data.List (concat, unwords)

{- Elementary parser combinataors. -}

document  = sepBy paragraph eop

paragraph = sepBy sentence eos

sentence  = sepBy word seppr -- (oneOf " \n") 

word      = many letter

seppr     =    try space 
           <|> try (char ',')
           <|> try (char '\n')
           <?> "word separator"


{-  It might be useful to get this rolling for a more flexible eos
eos       =    try (string ". ")
           <|> try (string "! ") 
           <|> try (string "? ")
           <?> "end of sentence"
-}

eop = string "<para>"

eos = oneOf ".?!" -- end of sentence

{-| -}
parseParagraph :: String -> Either ParseError [[String]]
parseParagraph = parse paragraph "unknown" 

parseMultiPara :: String ->  Either ParseError [[[String]]]
parseMultiPara = parse document "unknown"
