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
clientName client = case client of
                      GovOrg     name       -> name
                      Company    name _ _ _ -> name
                      Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing
