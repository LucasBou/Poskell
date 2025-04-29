module Cards where

data Color = Hearts | Spades | Diamonds | Clubs


data Value = Num Int | Jack | Queen | King | Ace deriving (Ord, Eq)

data Card = Card { color :: Color, value :: Value} 
instance Eq Card  where 
    Card _ valuea == Card _ valueb = valuea == valueb
    

instance Ord Card where
    Card _ valuea <= Card _ valueb = valuea<=valueb
