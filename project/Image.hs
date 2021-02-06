module Image where

data Image a = Image { width :: Int
                     , height :: Int
                     , content :: [[a]] } deriving (Show,Read,Eq)
