--file: ch04/FirstWord.hs
import System.Environment (getArgs)
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed."
        myFunction = transposeString

transposeLines :: [String] -> [String] -> [String]
transposeLines acc [] = acc
transposeLines acc (pre : suf) = transposeLines (transposeLine acc pre) suf

transposeLine acc "" = reverse acc
transposeLine (ac:acc) (s:str) = ((s:ac):(transposeLine acc str))

transposeString :: String -> String
transposeString [] = []
transposeString input = unlines (transposeLines acc normalizedLines)
    where inputLines = lines input
          maxLength = maximum (map length inputLines)
          acc = initializeEmptyStringList maxLength
          normalizedLines = unifyLines maxLength inputLines

unifyLines :: Int -> [String] -> [String]
unifyLines upToLength [str] = (str ++ (getEmptyString (upToLength - (length str))):[])
unifyLines upToLength [str:strs] = ((unifyLines upToLength str) : (unifyLines upToLength strs))

getEmptyString n | n == 0 = []
getEmptyString n = ' ' : getEmptyString n-1

initializeEmptyStringList :: Int -> [String]
initializeEmptyStringList 0 = ""
initializeEmptyStringList num = "":initializeEmptyStringList num-1
