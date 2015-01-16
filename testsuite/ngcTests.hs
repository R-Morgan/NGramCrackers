import Test.QuickCheck
import NGramCrackers.ListManipulation

{-| Test whether all bigrams are 2 words long 
quickCheck (\s -> all ( == 2) (map length $ bigrams s))


quickCheck (\s -> all (==2) (map length $ getNSeqFromList 2 s)) 

quickCheck (\n s -> all (==n) (map length $ getNSeqFromList n s)) 

quickCheck (\n s -> all (==n) (map length $ getNSeqFromString n s)) 

| Test that getNSeqFromList 2 returns lists of length 2 -}

genInts :: Gen Int
genInts = choose (1, 7)

getStringList :: Gen [String]
getStringList = do lst <- listOf $ listOf $ elements ['a'..'z']
                   return lst

genIntsStrings :: Gen (Int, [String])
genIntsStrings = do nSeq <- choose (1, 7)
                    lst <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']
                    return (nSeq, lst)

getNSeqProp n s =  all (== n) (map length $ getNSeqFromList n s)
