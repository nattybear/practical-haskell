module Chapter2.DataTypes where

data Client = GovOrg     String
            | Company    String Integer String String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

-- Time machines are defined by
-- their manufacturer,
-- their model (which is an integer),
-- their name,
-- whether they can travel to the past and to the future,
-- and a price
-- (which can be represented as a floating-point number)
data TimeMachine = TimeMachine String Integer String Time Double

data Time = Past | Future

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
discount percent (TimeMachine manufacturer model name time price) =
  TimeMachine manufacturer model name time newPrice
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
