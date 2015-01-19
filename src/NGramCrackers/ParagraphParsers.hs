module NGramCrackers.ParagraphParsers (
  parseParagraph
, parseMultiPara
) where


import Data.List (concat, unwords)
import Data.Text
import Text.ParserCombinators.Parsec

{- Elementary parser combinataors. -}

document :: Parser [[[String]]]
document = endBy paragraph eop

paragraph :: Parser [[String]]
paragraph = endBy sentence eos

sentence :: Parser [String]
sentence  = sepBy word seppr -- (oneOf " \n") 

word :: Parser String
word      = many letter

seppr :: Parser Char
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

eop :: Parser String
eop = string "<para>"

eos :: Parser Char
eos       = oneOf ".?!" -- end of sentence

{-| -}
parseParagraph :: String -> Either ParseError [[String]]
parseParagraph = parse paragraph "unknown" 

parseMultiPara :: String ->  Either ParseError [[[String]]]
parseMultiPara = parse document "unknown" 

 
