module P11CurrencySpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P11Currency hiding (main)
import Text.Printf (printf)


main :: IO ()
main = hspec spec

spec = do
  describe "getDollars" $ do
    prop "has the property of showing twice any number when the rateTo is 200 (i.e. 200%)" $
      \x -> getDollars x 200 `shouldBe` printf "%0.2f" (2*x)
    prop "has the property of showing half any number when the rateTo is 50" $
      \x -> getDollars x 50 `shouldBe` printf "%0.2f" (0.5*x)
  describe "rateTo" $ do
    prop "has the property of of converting any amount of euros valued at $200 to the equivalent exchange rate" $
      \x -> rateTo "200" x `shouldBe` (x/200) * 100
