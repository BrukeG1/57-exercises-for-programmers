module P33Magic8BallSpec (main,spec) where

import Test.Hspec
import P33Magic8Ball hiding (main)

main :: IO ()
main = hspec spec

--TODO: Implement tests for guessNum (hard because IO)
spec :: Spec
spec = do
    describe "showMsg" $ do
      it "should give correct message for 0" $ do
        showMsg 0 `shouldBe` "Yes"
      it "should give correct message for 1" $ do
        showMsg 1 `shouldBe` "No"
      it "should give correct message for 2" $ do
        showMsg 2 `shouldBe` "Maybe"
      it "should give correct message for 3" $ do
        showMsg 3 `shouldBe` "Ask again later"
--      it "should give error for -1" $ do
--        showMsg -1 `shouldThrow` 
--      it "should give error for -1" $ do
--        showMsg 4 `shouldThrow` 

