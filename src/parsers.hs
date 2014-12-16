import Text.ParserCombinators.Parsec
import Data.Either.Unwrap (mapRight)
import Data.List (concat, unwords)
import NGramCrackers.NGramCrackers

{- Elementary parser combinataors. -}
paragraph = endBy sentence eos
sentence = sepBy word (oneOf " \n") 
word     = many (noneOf " .?!\n") 
eos = oneOf ".?!"

{-| -}
parseParagraph :: String -> Either ParseError [[String]]
parseParagraph input = parse paragraph "(unknown)" input

{-| -}
flattenEither :: Either a [[b]] -> Either a [b]
flattenEither xs = mapRight concat xs

{-| -}
mapUnwords :: [[String]] -> [String]
mapUnwords  = map unwords

mapNGrams :: (String -> [String]) -> [String] -> [[String]]
mapNGrams nGramFunc sents = map nGramFunc sents

mapBigrams :: [String] -> [[String]]
mapBigrams = map bigrams

countWord :: String -> [String] -> (String, Int) 
countWord x xs = (x, count) where 
                              count = length $ filter (= x) xs

-- flattenEither e _  = 

{-
csvFile = endBy line eol
line = sepBy cell (char ' ')
cell = many (noneOf " \n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input


Either a, b => m a -> (a -> m b) -> m b

Right [a] 

\x -> Right b (Right [a])

{-This lambda function takes x and returns Right b. x does not seem to have aun 
  -}
-}
