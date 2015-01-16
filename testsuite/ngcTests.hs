import Test.QuickCheck
import NGramCrackers.ListManipulation


{-| Generator for generating valid Ints and Strings to test with the
    geNSeqProp property -}
genIntsStrings :: Gen (Int, [String])
genIntsStrings = do nSeq <- choose (1, 7)
                    lst <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']
                    return (nSeq, lst)

{-| Test that getNSeqFromList 2 returns lists of length 2 -}
getNSeqProp n s =  all (== n) (map length $ getNSeqFromList n s)
