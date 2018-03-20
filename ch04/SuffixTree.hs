--file: ch04/SuffixTree.hs
import Data.Char
import Data.List (tails)

suffixes :: [a] -> [[a]]
suffixes xs@(_x:xs') = xs : suffixes xs'
suffixes _ = []

noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _ = []

suffixes2 xs = init (tails xs)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes3 xs = compose init tails xs

suffixes4 = compose init tails
suffixes5 = init . tails

capCount = length . filter (isUpper . head) . words
