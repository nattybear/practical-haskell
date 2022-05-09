{-# LANGUAGE LambdaCase #-}
module Chapter3.Lists where

-- import Chapter2.DataTypes
import Chapter3.ParamPoly
import Data.List

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax :: Ord a => InfNumber a -> InfNumber a -> InfNumber a
infMax MinusInfinity x             = x
infMax x             MinusInfinity = x
infMax PlusInfinity  _             = PlusInfinity
infMax _             PlusInfinity  = PlusInfinity
infMax (Number a)    (Number b)    = Number (max a b)

maximum' :: [Integer] -> Integer
maximum' = foldr1 max

product' :: [Integer] -> Integer
product' = foldr (*) 1

-- minimumClient :: [Client] -> Client
-- minimumClient = foldr1 (\x y -> if f x < f y then x else y)
--   where f = length . clientName

all' :: [Bool] -> Bool
all' = foldr (&&) True

minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy g = foldr1 (\x y -> if g x < g y then x else y) 

bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
bothFilters p list = (filter p list, filter (not . p) list)

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case GovOrg {} -> False
                                _         -> True)

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False

checkAnalytics :: [Client a] -> (Bool, Bool)
checkAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

elem' :: Eq a => a -> [a] -> Bool
elem' x xs = case find (==x) xs of
               Just x' -> True
               Nothing -> False
