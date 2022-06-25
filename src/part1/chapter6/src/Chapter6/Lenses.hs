module Chapter6.Lenses where

data Client i = GovOrg     i String
              | Company    i String Person String
              | Individual i Person

data Person = Person String String
