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

foldl' _ zero [] = zero
foldl' step zero (x:xs) =
  let new = step zero x
  in new `seq` foldl' step new xs

-- incorrect: seq is hidden by the application of someFunc
-- since someFunc will be evaluated first, seq may occur too late
hiddenInside x y = someFunc (x `seq` y)

-- incorrect: a variation of the above mistake
hiddenByLet x y z = let a = x `seq` someFunc y
                        in anotherFunc a z

-- correct: seq will be evaluated first, forcing evaluation of x
onTheOutside x y = x `seq` someFunc y

chained x y z = x `seq` y `seq` someFunc z

badExpression step zero (x:xs) =
  seq (step zero x)
    (badExpression step (step zero x) xs)

strictPair (a,b) = a `seq` b `seq` (a,b)

strictList (x:xs) = x `seq` x : strictList xs
strictList [] = []
