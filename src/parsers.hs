import Text.ParserCombinators.Parsec
import Data.Either.Unwrap (mapRight)
import Data.List (concat, unwords)

paragraph = endBy sentence eos
sentence = sepBy word (oneOf " \n") 
word     = many (noneOf " .\n") 
eos = char '.'

{-| -}
parseParagraph :: String -> Either ParseError [[String]]
parseParagraph input = parse paragraph "(unknown)" input

{-| -}
flattenEither :: Either a [[b]] -> Either a [b]
flattenEither xs = mapRight concat xs

{-| -}
mapUnwords :: [[String]] -> [String]
mapUnwords  = map unwords

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
