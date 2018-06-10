import Data.Aeson
import Data.Aeson.TH

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq

d :: D Int
d = Record  { testOne = 3.14159
    , testTwo = True
    , testThree = Product "test" 'A' 123
            }

main = do
--    $(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''D)
    fromJSON (toJSON d) == Success d


