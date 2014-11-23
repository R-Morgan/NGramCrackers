module (
  getNGramsFromList
, getAlphasOnly
)

import Data.Char (isAlpha)

getNGramsFromString :: Int -> [Char] -> [Char]

getNGramsFromList :: Int -> [String] -> [[String]]
getNGramsFromList n wordList = getNSeqFromList n wordList 

{-| Return only alphabetic characters from a string and return the
    result as a string. Output of this function may need processing
    into a list, tuple, etc. -}
getAlphasOnly :: [Char] -> [Char]
getAlphasOnly = (unwords . map (filter (isAlpha)) . words)
