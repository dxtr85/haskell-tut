-- file: Exercises.hs
import Data.List (sortBy)
myLength :: [a] -> Int

myLength (_:xs) = 1 + myLength xs
myLength [] = 0


mean (x:xs) =  (fromIntegral sumList) /(fromIntegral lenList)
  where sumList = (x + (sum xs))
        lenList = (1 + (myLength xs))
mean [] = 0.0

palindrome xs = xs ++ (reverse xs)

isPalindrome [a] = True
isPalindrome (a:as) | a == z = isPalindrome middle
  where z = last as
        middle = reverse (drop 1 (reverse as))
isPalindrome [] = True
isPalindrome _ = False

compListByLength l1 l2 = compare (length l1) (length l2)
sortListByLength (x:xs) = sortBy compListByLength (x:xs)

join a [] = ""
join a [b] = b
join a (b:bs) = b ++ (a : (join a bs))

-- Exercise 8 in file Tree.hs

data Direction a = DLeft
                 | DRight
                 | DStraight
                 deriving (Show)

data Point a = Point a a
  deriving (Show)

px (Point a b) = a
py (Point a b) = b

direction p1 p2 p3 | det < 0 = DLeft
                                           | det == 0= DStraight
                                           | det > 0 = DRight
  where det1 = ((px p1) - (px p2))*((py p3) - (py p2))
        det2 = ((px p3) - (px p2))*((py p1) - (py p2))
        det  = det1 - det2

dirList (p1:p2:p3:ps) = ((direction p1 p2 p3):(dirList (p2:p3:ps)))
dirList _ = []

--minY (p1:ps) =


minYa a (p1:ps) | (py p1) < (py a) = minYa p1 ps
minYa a (p1:ps) | (py a) == (py p1) && (px p1) < (px a) = minYa p1 ps
minYa a (p1:ps) = minYa a ps
minYa a [] = a

vector a b = Point (pbx - pax) (pby - pay)
                 where pax = (px a)
                       pay = (py a)
                       pbx = (px b)
                       pby = (py b)

scalarMult a b = pax * pbx + pay * pby
                 where pax = (px a)
                       pay = (py a)
                       pbx = (px b)
                       pby = (py b)

distance a b = (sqrt ((pax - pbx) * (pax - pbx) +
                     (pay - pby) * (pay - pby)))
                 where pax = (px a)
                       pay = (py a)
                       pbx = (px b)
                       pby = (py b)
cosinus a b c = (scalarMult (vector a b) (vector b c))/
                ((distance a b) * (distance b c))
