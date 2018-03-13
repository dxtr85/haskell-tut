-- file: ch04/Fold.hs
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl step zero (x:xs) = myFoldl step (step zero x) xs
myFoldl _ zero [] = zero


myFoldr :: (a -> b -> b) -> b -> [a] -> b

myFoldr step zero (x:xs) = step x (myFoldr step zero xs)
myFoldr _ zero [] = zero


myFilter :: (a -> Bool) -> [a] -> [a]

myFilter p [] = []
myFilter p (x:xs)
  | p x = x : myFilter p xs
  | otherwise = myFilter p xs

myFilter2 p xs = foldr step [] xs
  where step x ys | p x = x: ys
                  | otherwise = ys

myMap :: (a -> b) -> [a] -> [b]

myMap f xs = myFoldr step [] xs
  where step x ys = f x : ys


myFoldl2 :: (a -> b -> a) -> a -> [b] -> a

myFoldl2 f z xs = myFoldr step id xs z
  where step x g a = g (f a x)

identity :: [a] -> [a]
identity xs = foldr (:) [] xs

append :: [a] -> [a] -> [a]

append xs ys = foldr (:) ys xs
