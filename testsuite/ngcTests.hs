import Test.QuickCheck
import NGramCrackers.ListManipulation

{-| Test whether all bigrams are 2 words long 
quickCheck (\s -> all ( == 2) (map length $ bigrams s))


quickCheck (\s -> all (==2) (map length $ getNSeqFromList 2 s)) 

quickCheck (\n s -> all (==n) (map length $ getNSeqFromList n s)) 

quickCheck (\n s -> all (==n) (map length $ getNSeqFromString n s)) 

| Test that getNSeqFromList 2 returns lists of length 2 -}

getNSeqProp n s = n <= length s && n > 0 && n < 7 ==> all (== n) (map length $ getNSeqFromList n s)
  where types = s::[String]
