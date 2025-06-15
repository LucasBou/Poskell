module Cards where
import Data.String( IsString(..) )
import Data.Maybe ( fromJust)

data Color = Hearts | Spades | Diamonds | Clubs deriving (Eq,Ord,Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten| Jack | Queen | King | Ace deriving (Ord, Eq,Show, Enum)

data Card = Card { color :: Color, value :: Value} deriving (Show)

instance Eq Card  where 
    Card _ valuea == Card _ valueb = valuea == valueb

instance Ord Card where
    Card _ valuea <= Card _ valueb = valuea<=valueb

instance IsString Card where
    fromString (firstChar : [secondChar]) = Card (fromJust $ colorFromChar secondChar) (fromJust $ valueFromChar firstChar)
    fromString ('1' :'0' :[colorChar]) = Card (fromJust $ colorFromChar colorChar) Ten
    fromString _ = undefined "unexpected string while constructing a Card"

valueFromChar :: Char -> Maybe Value
valueFromChar '2' = Just Two 
valueFromChar '3' = Just Three 
valueFromChar '4' = Just Four 
valueFromChar '5' = Just Five 
valueFromChar '6' = Just Six 
valueFromChar '7' = Just Seven 
valueFromChar '8' = Just Eight 
valueFromChar '9' = Just Nine 
valueFromChar 'J' = Just Jack 
valueFromChar 'Q' = Just Queen 
valueFromChar 'K' = Just King 
valueFromChar 'A' = Just Ace 
valueFromChar  _  = Nothing 

colorFromChar :: Char -> Maybe Color
colorFromChar '♥' = Just Hearts
colorFromChar '♠' = Just Spades
colorFromChar '♦' = Just Diamonds
colorFromChar '♣' = Just Clubs
colorFromChar  _  = Nothing

