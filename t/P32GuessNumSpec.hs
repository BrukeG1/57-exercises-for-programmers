module P32GuessNumSpec (main,spec) where

import Test.Hspec
import P32GuessNum hiding (main)

main :: IO ()
main = hspec spec

--TODO: Implement tests for guessNum (hard because IO)
spec :: Spec
spec = do
    describe "winMsg" $ do
      it "should give correct message for 0" $ do
        winMsg 0 `shouldBe` "You got it in 1 guess! You're a mindreader."
      it "should give correct message for -1" $ do
        winMsg (-1) `shouldBe` "You got it in -1. Most impressive."
      it "should give correct message for 2" $ do
        winMsg 2 `shouldBe` "You got it in 2. Most impressive."
      it "should give correct message for 3" $ do
        winMsg 3 `shouldBe` "You got it in 3. You can do better than that."
      it "should give correct message for 5" $ do
        winMsg 5 `shouldBe` "You got it in 5. You can do better than that."
      it "should give correct message for 6" $ do
        winMsg 6 `shouldBe` "You got it in 6. Better luck next time."
    describe "hint" $ do
      it "should give correct message for g==a" $ do
        hint 1 1 `shouldBe` "Too high!" -- though this should never happen
      it "should give correct message for g>a" $ do
        hint 5 1 `shouldBe` "Too high!"
      it "should give correct message for g<a" $ do
        hint 1 5 `shouldBe` "Too low!"
--  describe "guessNum" $ do
--    it "gives true on correct guess on first go" $ do
--      guessNum [] 1 1 `shouldBe` true
