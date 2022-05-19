{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter4.Containers where

import           Chapter3.ParamPoly
import           Data.Graph
import qualified Data.Map           as M
import qualified Data.Set           as S
import           Data.Tree

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v = M.alter (\_ -> Just v) k

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete = M.alter (\_ -> Nothing)

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f = M.alter (\case Nothing -> Nothing
                          Just v  -> Just (f v))

data ClientKind = GovOrgKind
                | CompanyKind
                | IndividualKind
                deriving (Eq, Ord, Show)

classifyClients :: [Client Integer]
                -> M.Map ClientKind (S.Set (Client Integer))
classifyClients clients = M.fromList [ (GovOrgKind,     S.fromList xs)
                                     , (CompanyKind,    S.fromList ys)
                                     , (IndividualKind, S.fromList zs) ]
  where go (c:cs) (xs, ys, zs) =
          case c of
            GovOrg     { .. } -> go cs (c:xs, ys, zs)
            Company    { .. } -> go cs (xs, c:ys, zs)
            Individual { .. } -> go cs (xs, ys, c:zs)
        (xs, ys, zs) = go clients ([], [], [])

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees)
  = let subtreesTraversed = concat $ map (preOrder f) subtrees
    in f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                              , Node 6 [] ]

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [("wood","wood",["walls"])
  ,("plastic","plastic",["walls","wheels"])
  ,("aluminum","luminum",["wheels","door"])
  ,("walls","walls",["done"])
  ,("wheels","wheels",["done"])
  ,("door","door",["done"])
  ,("done","done",[])]

timeMachinePrecedence :: ( Graph
                         , Vertex -> (String,String,[String])
                         , String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph
