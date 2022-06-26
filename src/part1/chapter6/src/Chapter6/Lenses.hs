module Chapter6.Lenses where

import Lens.Micro.Platform

data Client i = GovOrg     i String
              | Company    i String Person String
              | Individual i Person

data Person = Person String String

firstName :: Lens' Person String
firstName = lens (\(Person f _) -> f)
                 (\(Person _ l) newF -> Person newF l)
