module Chapter3.Lists where

import Chapter2.DataTypes

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

minimumClient :: [Client] -> Client
minimumClient = foldr1 (\x y -> if f x < f y then x else y)
  where f = length . clientName

all' :: [Bool] -> Bool
all' = foldr (&&) True

minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy g = foldr1 (\x y -> if g x < g y then x else y) 

bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
bothFilters p list = (filter p list, filter (not . p) list)
