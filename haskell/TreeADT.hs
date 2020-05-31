data BinTree a = BinNode a (BinTree a) (BinTree a)
            | BinEmpty
              deriving (Show)
simpleBinTree = BinNode "parent" (BinNode "left child" BinEmpty BinEmpty)
                                 (BinNode "right child" BinEmpty BinEmpty)

data Tree a = Node a (Maybe [Tree a])
              deriving (Show)
myTree = Node 1 (Just [Node 2 Nothing, Node 3 Nothing])
-- Guard allows us to pattern match only trees who have same first node
nodesAreSame (Node a _) (Node b _)
    | a == b     =  a
    | a /= b     = error "nodes are not the same"
