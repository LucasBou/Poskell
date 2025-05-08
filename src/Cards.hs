module Cards where
import Data.List
data Color = Hearts | Spades | Diamonds | Clubs deriving (Eq,Ord,Show)


data Value = Num Int | Jack | Queen | King | Ace deriving (Ord, Eq,Show)

data Card = Card { color :: Color, value :: Value} deriving (Show)
instance Eq Card  where 
    Card _ valuea == Card _ valueb = valuea == valueb
    

instance Ord Card where
    Card _ valuea <= Card _ valueb = valuea<=valueb

type Cards = [Card]

extractColor :: Card->Color
extractColor Card {color=col,value=_} = col

extractListColors:: Cards ->[Color]
extractListColors cards = map extractColor cards

sortByColor :: Cards->[Color]
sortByColor cards = sort (extractListColors cards)


isFlush :: Cards->Bool
isFlush cards = head (sortByColor cards )== sortByColor cards !!4
