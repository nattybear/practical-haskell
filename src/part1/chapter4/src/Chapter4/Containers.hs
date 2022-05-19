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
classifyClients clients = go clients m'
  where go (x:xs) m =
          case x of
            GovOrg     { .. } -> M.adjust (S.insert x) GovOrgKind     m
            Company    { .. } -> M.adjust (S.insert x) CompanyKind    m
            Individual { .. } -> M.adjust (S.insert x) IndividualKind m
        m' = M.fromList [ (GovOrgKind,     s)
                        , (CompanyKind,    s)
                        , (IndividualKind, s) ]
        s  = S.empty
