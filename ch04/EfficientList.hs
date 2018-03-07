--file: ch04/EfficientList.hs
import Data.List

myDumbExample xs = if length xs > 0
                      then head xs
                   else 'Z'

mySmartExample xs = if not (null xs)
                    then head xs
                    else 'Z'
myOtherExample (x:_) = x
myOtherExample _ = 'Z'

listAppend = [1,2,3] ++ [4,5,6]
listConcatenation = concat [[1, 2, 3], [4, 5, 6]]
listReverse = reverse [1,2,3,4,5,6]
listAnd = and [True, True, False]
listAnd2 = and []
listOr = or [False, False, True]
listOr2 = or []
listCheck = all odd [1,3,5,6]
listCheck2 = all odd []
listCheck3 = any even [1,3,5,6]

listTake = take 3 "foobar"
listTake2 = take 2 []
listDrop = drop 5 [1,2,3,4,5,6,7]
listSplit = splitAt 3 "gumiak"
listTakeWhile = takeWhile odd [1,3,5,7,8,9]
listDropWhile = dropWhile odd [1,3,5,7,8,9]
listSpanEven = span even [2,4,6,7,8,9]
listBreakEven = break even [1,2,4,6,7,8,9]
listFilterEven = filter even [1,2,3,4,5]

listIsPrefix = "foo" `isPrefixOf` "foobar"
listIsInfix = "ojczyzno" `isInfixOf` "Litwo, ojczyzno moja!"
listIsSuffix = "ja!" `isSuffixOf` "Litwo, ojczyzno moja!"

listZip = zip [13,15,17] "dupa"
listZipWith = zipWith (+) [1,2,3,4] [99,98,97,96]
