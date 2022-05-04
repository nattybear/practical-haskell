{-# LANGUAGE LambdaCase #-}
module Chapter3.FnsParams where

import Chapter2.DataTypes

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

equalTuples :: [(Integer,Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\case "Alejandro" -> "Hello, writer"
                            name        -> "Welcome, " ++ name
                     ) names

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n*x

filterOnes :: (Eq a, Num a) => [a] -> [a]
filterOnes = filter (== 1)

filterANumber :: (Eq a, Num a) => a -> [a] -> [a]
filterANumber n = filter (== n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter isGovOrg
  where isGovOrg = \case GovOrg _ -> True
                         client   -> False
