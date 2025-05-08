module Cards where
import Data.List
data Color = Hearts | Spades | Diamonds | Clubs deriving (Eq,Ord,Show)


data Value = One |Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten| Jack | Queen | King | Ace deriving (Ord, Eq,Show, Enum)

data Card = Card { color :: Color, value :: Value} deriving (Show)
instance Eq Card  where 
    Card _ valuea == Card _ valueb = valuea == valueb
    

instance Ord Card where
    Card _ valuea <= Card _ valueb = valuea<=valueb

type Cards = [Card]

extractColor :: Card -> Color
extractColor Card {color=col,value=_} = col

extractValue :: Card -> Value
extractValue Card {color=_,value=val} = val

extractListColors:: Cards -> [Color]
extractListColors cards = map extractColor cards

extractListValues:: Cards -> [Value]
extractListValues cards = map extractValue cards

sortByColor :: Cards->[Color]
sortByColor cards = sort (extractListColors cards)

sortByValue :: Cards -> [Value]
sortByValue cards = sort (extractListValues cards)

highest_card::Cards -> Value
highest_card cards = last (sortByValue(cards))

isFlush :: Cards->Bool
isFlush cards = sortByColor cards !!0== sortByColor cards !!4

isStraight :: Cards->Bool
isStraight cards = sortByValue cards !!4 == succ (succ (succ (succ (sortByValue cards !!0))))

isFlushStraight :: Cards->Bool
isFlushStraight cards = isStraight cards && isFlush cards 

isFourOfAKind :: Cards -> Bool
isFourOfAKind cards = (sortByValue cards !! 0 == sortByValue cards !!3 ) || (sortByValue cards !! 1 == sortByValue cards !!4 )

isThreeOfAKind:: Cards -> Bool
isThreeOfAKind cards = (sortByValue cards !! 0 == sortByValue cards !!2 ) || (sortByValue cards !! 1 == sortByValue cards !!3 ) || (sortByValue cards !! 2 == sortByValue cards !!4 )

isPair:: Cards -> Bool
isPair cards = (sortByValue cards !! 0 == sortByValue cards !!1 ) || (sortByValue cards !! 1 == sortByValue cards !!2 ) || (sortByValue cards !! 2 == sortByValue cards !!3 )|| (sortByValue cards !! 3 == sortByValue cards !!4 )

isTwoPairs:: Cards->Bool
isTwoPairs cards = not(isFourOfAKind cards) && (((sortByValue cards !! 0 == sortByValue cards !!1 ) && (sortByValue cards !! 2 == sortByValue cards !!3 )) ||((sortByValue cards !! 0 == sortByValue cards !!1 ) && (sortByValue cards !! 3 == sortByValue cards !!4 )) ||((sortByValue cards !! 3 == sortByValue cards !!4 ) && (sortByValue cards !! 2 == sortByValue cards !!3 )))

isFull:: Cards -> Bool
isFull cards = ((sortByValue cards !! 0 == sortByValue cards !! 2) && (sortByValue cards !! 3 == sortByValue cards !! 4)) ||((sortByValue cards !! 0 == sortByValue cards !! 1) && (sortByValue cards !! 2 == sortByValue cards !! 4))