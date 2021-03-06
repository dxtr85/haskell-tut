-- file: ch05/PrettyStub.hs
import SimpleJSON

data Doc = ToBeDefined
  deriving (Show)

string :: String -> Doc
string str = undefined

text  :: String -> Doc
text str = undefined

double :: String -> Doc
double num = undefined


(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p b = undefined

compact :: Doc -> [Doc] -> [Doc]
compact p b = undefined

