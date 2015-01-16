import Test.QuickCheck
import NGramCrackers.ListManipulation


{-| Generator for generating valid Ints and Strings to test with the
    geNSeqProp property -}
genIntsStrings :: Gen (Int, [String])
genIntsStrings = do nSeq <- choose (1, 7)
                    lst <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']
                    return (nSeq, lst)

{-| Test that getNSeqFromList returns lists of length n -}
getNSeqProp n s =  all (== n) (map length $ getNSeqFromList n s)
