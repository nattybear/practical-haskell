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
minimumClient [x]      = x
minimumClient (x:y:xs) = if f x < f y
                         then minimumClient (x:xs)
                         else minimumClient (y:xs)
  where f = length . clientName
