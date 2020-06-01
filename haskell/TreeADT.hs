data BinTree a = BinNode a (BinTree a) (BinTree a)
            | BinEmpty
              deriving (Show)
-- height 2
simpleBinTree = BinNode "parent" (BinNode "left child" BinEmpty BinEmpty)
                                 (BinNode "right child" BinEmpty BinEmpty)
-- height 4               
complexBinTree = BinNode "great grand parent"
                 (BinNode "grand parent" simpleBinTree BinEmpty)
                 (BinNode "grand parent" BinEmpty BinEmpty)
                 

binTreeHeight (BinNode a b c) = 1 + (max (binTreeHeight b) (binTreeHeight c))
binTreeHeight BinEmpty = 0

data Tree a = Node a (Maybe [Tree a])
              deriving (Show)
myTree = Node 1 (Just [Node 2 Nothing, Node 3 Nothing])
-- Guard allows us to pattern match only trees who have same first node
nodesAreSame (Node a _) (Node b _)
    | a == b     = a
    | a /= b     = error "nodes are not the same"
