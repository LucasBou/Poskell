module Cards where

data Color = Hearts | Spades | Diamonds | Clubs deriving(Show)


data Value = Num Int | Jack | Queen | King | Ace deriving (Ord, Eq, Show)

data Card = Card { color :: Color, value :: Value} deriving(Show)

instance Eq Card  where 
    Card _ valuea == Card _ valueb = valuea == valueb
    

instance Ord Card where
    Card _ valuea <= Card _ valueb = valuea<=valueb

data Hand = High Value | Pair Value | TwoPair Value Value | ThreeOfAKind Value | Straight Value | Flush Value | FullHouse Value Value | FourOfAKind Value | StraightFlush Value deriving(Eq, Ord, Show)
