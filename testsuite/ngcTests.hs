import Test.QuickCheck
import NGramCrackers.NGramCrackers

{-| Test whether all bigrams are 2 words long -}
quickCheck (\s -> all ( == 2) (map length $ bigrams s))


{-| Test that getNSeqFromList 2 returns lists of length 2 -}
quickCheck (\s -> all (==2) (map length $ getNSeqFromList 2 s)) 

quickCheck (\n s -> all (==n) (map length $ getNSeqFromList n s)) 

quickCheck (\n s -> all (==n) (map length $ getNSeqFromString n s)) 


