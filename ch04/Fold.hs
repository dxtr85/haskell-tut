-- file: ch04/Fold.hs
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl step zero (x:xs) = myFoldl step (step zero x) xs
myFoldl _ zero [] = zero

myFoldr :: (a -> b -> b) -> b -> [a] -> b

myFoldr step zero (x:xs) = step x (foldr step zero xs)
myFoldr _ zero [] = zero
