module NGramCrackers.ParagraphParsers (
  parseParagraph
, parseMultiPara
, flattenEither
) where

import Text.ParserCombinators.Parsec
import Data.Either.Unwrap (mapRight)
import Data.List (concat, unwords)

{- Elementary parser combinataors. -}

--paragraphFile = do 
                  
document = sepBy paragraph eop

paragraph = sepBy sentence eos

sentence  = sepBy word seppr -- (oneOf " \n") 

word      = many (noneOf " .?!\n") 

seppr     =    try space 
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

--parseParagraphFile :: String -> Either ParseError [[String]]
--parseParagraphFile input = parse paragraphFile "(unknown)" input 

{-| -}
flattenEither :: Either a [[b]] -> Either a [b]
flattenEither = mapRight concat

-- Test code for writing a csv file after parsing
{- 
   case parseParagraph input of 
         Left e  -> do putStrLn "Error parsing input: "
                       print e

         Right r -> hPutStrLn outHandle "word,count" >> 
                    mapM_ (hPutStrLn outHandle . doubleToCSV) (lexemeCountProfile $ concat r)

-}
