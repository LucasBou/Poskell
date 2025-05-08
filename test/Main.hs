module Main (main) where
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Cards (Value(..), Color(..), Card(..), isFlush,isStraight)

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
    detectStraight1
    detectStraight2
    detectNotStraight1
    detectNotStraight2

dummyTest = it "ensures the test suite runs" $ do
  1 `shouldBe` 1

queenBetterThanJack = it "ensures a queen beats a jack" $ do
    Jack < Queen  `shouldBe` True

queenDiamondEqualQueenHeart =  it "ensures that a queen diamond is equal to queen heart" $ do
    Card Diamonds Queen == Card Hearts Queen `shouldBe` True

aceHeartNotEqualToEightClubs = it "ensures that a ace heart is not equal to eight clubs" $ do
    Card Hearts Ace /= Card Clubs  Eight `shouldBe` True

tenClubsBetterThanthreeHearts = it "ensures that 10 clubs us better than three hearts" $ do
  Card Clubs Ten> Card Hearts Three `shouldBe` True

detectFlush = it "ensures that 2, 3, 7,8, and 10 of spades is a flush" $ do
  isFlush [Card Spades Two, Card Spades Three, Card Spades Seven, Card Spades Eight, Card Spades Ten] `shouldBe` True

detectNotFlush1 = it "ensures that 2, 3, 7,8 of spades + Jack of heart is not a flush" $ do
  isFlush [Card Spades Two, Card Spades Three, Card Spades Seven, Card Spades Eight, Card Hearts Jack] `shouldBe` False

detectNotFlush2 = it "ensures that 2 of heart + ace of spades + king of clubs + queen of diamons + Jack of heart is not a flush" $ do
  isFlush [Card Hearts Two, Card Spades Ace, Card Clubs King, Card Diamonds Queen, Card Hearts Jack] `shouldBe` False

detectStraight1 = it "ensures that 2 of heart, 3 of spades, 4 of diamonds, 5 of clubs and 6 of spades is a straight" $ do
  isStraight [Card Hearts Two, Card Spades Three, Card Diamonds Four, Card Clubs Five, Card Spades Six] `shouldBe` True

detectStraight2 = it "ensures that 10 of heart, jack of spades, queen of diamonds, king of clubs and ace of spades is a straight" $ do
  isStraight [Card Hearts Ten, Card Spades Jack, Card Diamonds Queen, Card Clubs King, Card Spades Ace] `shouldBe` True


detectNotStraight1 = it "ensures that 2 of heart, 3 of spades, 4 of diamonds, Jack of clubs and 6 of spades is a straight" $ do
  isStraight [Card Hearts Two, Card Spades Three, Card Diamonds Four, Card Clubs Jack, Card Spades Six] `shouldBe` False


detectNotStraight2 = it "ensures that 10 of heart, jack of spades, queen of diamonds, king of clubs and two of spades is a straight" $ do
  isStraight [Card Hearts Ten, Card Spades Jack, Card Diamonds Queen, Card Clubs King, Card Spades Two] `shouldBe` False