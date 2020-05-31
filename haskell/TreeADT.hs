data BinTree a = BinNode a (BinTree a) (BinTree a)
            | BinEmpty
              deriving (Show)
simpleBinTree = BinNode "parent" (BinNode "left child" BinEmpty BinEmpty)
                                 (BinNode "right child" BinEmpty BinEmpty)

data Tree a = Node a [Tree (Maybe a)]
              deriving (Show)

