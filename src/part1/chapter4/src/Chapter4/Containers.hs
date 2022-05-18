{-# LANGUAGE LambdaCase #-}
module Chapter4.Containers where

import qualified Data.Map as M

-- M.alter :: Ord k => (Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v = M.alter (\_ -> Just v) k

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete = M.alter (\_ -> Nothing)

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f = M.alter (\case Nothing -> Nothing
                          Just v  -> Just (f v))
