-- file: ch04/Plus.hs
import Data.List

a `plus` b = a + b

data a `Pair` b = a `Pair` b
                deriving (Show)

-- we can use the constructor either prefix or infix
foo = Pair 1 2
bar = True `Pair` "quux"

isPref = "foo" `isPrefixOf` "foobar"
isInfix = "needle" `isInfixOf` "haystack full of needle thingies"
end = "end" `isSuffixOf` "the end"
