-- file: ch03/Nullable.hs
data Maybe a = Juste a
             | Nothing
someBool = Juste True

someString = Juste "something"
wrapped = Just (Just "wrapped")
