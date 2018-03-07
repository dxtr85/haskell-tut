--file: ch04/Exercises.hs
import Data.Char (isUpper)
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
