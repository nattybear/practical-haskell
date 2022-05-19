{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter4.Containers where

import Chapter3.ParamPoly
import qualified Data.Map as M
import qualified Data.Set as S

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
