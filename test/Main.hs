module Main (main) where
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Cards (Value(..), Color(..), Card(..), isFlush)

main :: IO ()
main = hspec globalSpec

globalSpec :: Spec
globalSpec = do
  describe "The Poskell tests for the library" $ do
    dummyTest
    queenBetterThanJack
    queenDiamondEqualQueenHeart
    aceHeartNotEqualToEightClubs
    tenClubsBetterThanthreeHearts
    detectFlush
    detectNotFlush1
    detectNotFlush2

dummyTest = it "ensures the test suite runs" $ do
  1 `shouldBe` 1

queenBetterThanJack = it "ensures a queen beats a jack" $ do
    Jack < Queen  `shouldBe` True

queenDiamondEqualQueenHeart =  it "ensures that a queen diamond is equal to queen heart" $ do
    Card Diamonds Queen == Card Hearts Queen `shouldBe` True

aceHeartNotEqualToEightClubs = it "ensures that a ace heart is not equal to eight clubs" $ do
    Card Hearts Ace /= Card Clubs  (Num 8) `shouldBe` True

tenClubsBetterThanthreeHearts = it "ensures that 10 clubs us better than three hearts" $ do
  Card Clubs (Num 10)> Card Hearts (Num 3) `shouldBe` True

detectFlush = it "ensures that 2, 3, 7,8, and 10 of spades is a flush" $ do
  isFlush [Card Spades (Num 2), Card Spades (Num 3), Card Spades (Num 7), Card Spades (Num 8), Card Spades (Num 10)] `shouldBe` True

detectNotFlush1 = it "ensures that 2, 3, 7,8of spades + Jack of heart is not a flush" $ do
  isFlush [Card Spades (Num 2), Card Spades (Num 3), Card Spades (Num 7), Card Spades (Num 8), Card Hearts Jack] `shouldBe` False

detectNotFlush2 = it "ensures that 2 of heart + ace of spades + king of clubs + queen of diamons + Jack of heart is not a flush" $ do
  isFlush [Card Hearts (Num 2), Card Spades Ace, Card Clubs King, Card Diamonds Queen, Card Hearts Jack] `shouldBe` False