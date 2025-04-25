module Main (main) where
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Cards (Value(..))

main :: IO ()
main = hspec globalSpec

globalSpec :: Spec
globalSpec = do
  describe "The Poskell tests for the library" $ do
    dummyTest
    queenBetterThanJack

dummyTest = it "ensures the test suite runs" $ do
  1 `shouldBe` 1

queenBetterThanJack = it "ensures a queen beats a jack" $ do
    Jack < Queen  `shouldBe` True