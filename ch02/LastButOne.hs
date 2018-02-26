-- file: ch02/LastButOne.hs

lastButOne xs = if length xs < 2
  then 0
  else if length xs == 2
       then head xs
       else lastButOne (tail xs)
