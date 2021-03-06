{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Chapter2.DataTypes where

import Data.Char

data Client = GovOrg     String
            | Company    String Integer String String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data TimeMachine = TimeMachine { manufacturer :: String
                               , model :: Integer
                               , name :: String
                               , time :: Time
                               , price :: Double
                               } deriving Show

data Time = Past | Future
          deriving Show

clientName :: Client -> String
clientName (GovOrg name)                     = name
clientName (Company name _ _ _)              = name
clientName (Individual (Person fNm lNm _) _) = fNm ++ " " ++ lNm

companyName :: Client -> Maybe String
companyName (Company name _ _ _) = Just name
companyName _                    = Nothing

data NumberOfClient = NumberOfClient Gender Int
                    deriving Show

numberOfClient :: Client -> Maybe NumberOfClient
numberOfClient (GovOrg _)                         = Nothing
numberOfClient (Company _ _ _ _)                  = Nothing
numberOfClient (Individual (Person _ _ gender) _) =
  Just (NumberOfClient gender 1)

discount :: Double -> TimeMachine -> TimeMachine
discount percent t@(TimeMachine { .. })= t { price = newPrice }
  where newPrice = price * (1 - percent)

discounts :: Double -> [TimeMachine] -> [TimeMachine]
discounts percent = map (discount percent)

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0 = Nothing
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n | otherwise = let Just f1 = ifibonacci (n-1)
                               Just f2 = ifibonacci (n-2)
                           in Just (f1 + f2)

binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise      = show n ++ " is a beautiful number"

ackermann :: Int -> Int -> Int
ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m-1) 1
  | m > 0 && n > 0  = ackermann (m-1) (ackermann m (n-1))

unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = (ys, zs)
  where foo ((x,_):zs) = x : foo zs
        bar ((_,y):zs) = y : bar zs
        ys = foo xs
        zs = bar xs

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _                               = False

data ClientR = GovOrgR  { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

greet :: ClientR -> String
greet IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet CompanyR    { .. }                      = "Hi, " ++ clientRName
greet GovOrgR     { }                         = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
  let newName = (toUpper initial):rest
  in  p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p
