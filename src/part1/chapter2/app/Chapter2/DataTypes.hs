module Chapter2.DataTypes where

data Client = GovOrg     String
            | Company    String Integer String String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data TimeMachine = String Integer String Time Double

data Time = Past | Future

clientName :: Client -> String
clientName (GovOrg name)                     = name
clientName (Company name _ _ _)              = name
clientName (Individual (Person fNm lNm _) _) = fNm ++ " " ++ lNm

companyName :: Client -> Maybe String
companyName (Company name _ _ _) = Just name
companyName _                    = Nothing
