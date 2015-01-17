import Test.QuickCheck
import NGramCrackers.ListManipulation

{-| Custom Args that increase the maximum number of successful tests
    from 100 to 1000 -}

myArgs = Args { replay = Nothing, maxSuccess = 1000, maxDiscardRatio = 10,
                chatty = True }

{-| Generator for generating valid Ints and Strings to test with the
    geNSeqProp property -}
genIntsStrings :: Gen (Int, [String])
genIntsStrings = do nSeq <- choose (1, 7)
                    lst <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']
                    return (nSeq, lst)

{-| Test that getNSeqFromList returns lists of length n -}
getNSeqProp n s =  all (== n) (map length $ getNSeqFromList n s)
