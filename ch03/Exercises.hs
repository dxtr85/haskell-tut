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
                 deriving (Show, Eq)

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

compPointsByY p1 p2 = compare (py p1) (py p2)
sortPointsByY (p:ps) = sortBy compPointsByY (p:ps)

-- 1. sort list by Y coord asc
-- 2. swap 1st and 2nd elems of sorted list
-- 3. add 1st point to the end of sorted list
-- 4. first two points are assumed to be the boundary (only second is, the first will be optionally added from point 3)
-- 5. compare last point that is in boundary with first two candidates if no more points to compare are left go to 9.
-- 6.   if the turn is right, the first candidate is inside boundary and should be discarded
--        (if these points create straight line it can be added to boundary)
-- 7. compare second point with  last two boundary points and if it turns left add it to boundary, go to 5.
-- 8.   in other case remove last boundary point from boundary-points set and go to 7.
-- 9. return boundary points excluding the first one

grahamScan (up1:up2:ups) = (init (grahamScanAlg accu rest))
  where (sp:sps) = sortPointsByY (up1:up2:ups)
        accu = (sp:(head sps):[])
        rest = (reverse ((head sps):(reverse (tail sps))))
grahamScan p = p


grahamScanAlg acc [] = acc

grahamScanAlg acc (p:[]) = grahamScanAlg2 acc (p:[])

grahamScanAlg acc (p1:ps) | dir == DLeft = grahamScanAlg (p1:acc) ps
  where dir = direction (head acc) p1 (head ps)

grahamScanAlg acc (p:ps) = grahamScanAlg2 acc ps


grahamScanAlg2 acc [] = acc

grahamScanAlg2 acc ps | dir == DLeft = grahamScanAlg ((head ps):acc) (tail ps)
  where ac1 = head acc
        ac2 = (head (tail acc))
        dir = direction ac2 ac1 (head ps)

grahamScanAlg2 acc ps | dir == DRight || dir == DStraight = grahamScanAlg2 (tail acc) ps
  where ac1 = head acc
        ac2 = (head (tail acc))
        dir = direction ac2 ac1 (head ps)

