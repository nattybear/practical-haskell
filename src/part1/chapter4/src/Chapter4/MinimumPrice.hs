module Chapter4.MinimumPrice where

data TravelGuide = TravelGuide { title   :: String
                               , authors :: [String]
                               , price   :: Double }
                 deriving (Show, Eq, Ord)

data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1
                 | Leaf1
                 deriving Show

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind1 t l
                              GT -> treeFind1 t r
treeFind1 _ Leaf1         = Nothing

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t n@(Node1 v l r) = case compare t v of
                                  EQ -> n
                                  LT -> Node1 v (treeInsert1 t l) r
                                  GT -> Node1 v l (treeInsert1 t r)
treeInsert1 t Leaf1           = Node1 t Leaf1 Leaf1

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2
                   deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind2 t l
                              GT -> treeFind2 t r
treeFind2 _ Leaf2         = Nothing

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
                                  EQ -> n
                                  LT -> Node2 v (treeInsert2 t l) r
                                  GT -> Node2 v l (treeInsert2 t r)
treeInsert2 t Leaf2           = Node2 t Leaf2 Leaf2

concat' :: BinaryTree1 -> BinaryTree1 -> BinaryTree1
concat' Leaf1         t = t
concat' (Node1 v l r) t = l `concat'` r `concat'` treeInsert1 v t

newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
  (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))
