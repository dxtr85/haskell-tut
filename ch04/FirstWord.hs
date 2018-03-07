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
        myFunction = firstWord

getFirstWord :: [String] -> [String]
getFirstWord [] = []
getFirstWord (pre : suf)| not (null pre) = (head (words pre)) : getFirstWord suf
getFirstWord (pre : suf) = getFirstWord suf

firstWord :: String -> String
firstWord [] = []
firstWord input = unlines (getFirstWord (lines input))
