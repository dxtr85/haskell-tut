--file: ch04/Exercises.hs
import Data.Char (isUpper, digitToInt, isDigit)
import Data.Either

safeHead :: [a] -> Maybe a

safeHead (x:_) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]

safeTail (_:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a

safeLast x = safeHead (reverse x)
safeInit [] = Nothing
safeInit x = Just (init x)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith splitFunc xs = case break splitFunc xs of
  ([], xs) -> splitWith splitFunc (tail xs)
  (yx, []) -> [yx]
  (yx, zs) -> [yx] ++ splitWith splitFunc (tail zs)

asInt_fold :: String -> Int
asInt_fold s | length s > 17 = error "String too long!."
asInt_fold ('-':s) = negate (asInt_fold s)
asInt_fold s = foldl step 0 s
  where step acc s | isDigit s = 10 * acc + (digitToInt s)
                   | otherwise = error "Not a valid Int string!"

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either s | length s > 17 = Left "String too long!."
asInt_either ('-':s) | all isDigit s = Right (negate (asInt_fold s))
asInt_either s | not (all (isDigit) s) = Left "Not a valid Int string!."
asInt_either s = Right (asInt_fold s)


concat_foldr :: [[a]] -> [a]
concat_foldr a = foldr (++) [] a

takeWhile_my :: (a -> Bool) -> [a] -> [a]
takeWhile_my _ [] = []
takeWhile_my cond (s:ss) | cond s = s : (takeWhile_my cond ss)
                         | otherwise = []

takeWhile_my2 :: (a -> Bool) -> [a] -> [a]
takeWhile_my2 cond s = foldr step [] s
  where step s acc | cond s = (s:acc)
                   | otherwise = []

groupByMy :: (a -> a -> Bool) -> [a] -> [[a]]
groupByMy cond s = foldr step [[]] s
  where step s accu@(a:acc) | not (null a) && not (s `cond` (head a)) = [s]:accu
                       | otherwise = (s:a):acc
