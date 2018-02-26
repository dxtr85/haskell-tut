-- file: ch03/TreeMaybe.hs
data TreeM a = TreeM a
               (Maybe (TreeM a)) (Maybe (TreeM a))
             deriving (Show)

simpleTreeMaybe = TreeM  "parent"
  (Just (TreeM "left child"  Nothing  Nothing))
  (Just (TreeM "right child" Nothing Nothing))
