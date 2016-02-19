module P01HelloSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P01Hello hiding (main)
 

main :: IO ()
main = hspec spec

spec = do
  describe "chooseGreet" $ do
    it "insults David Cameron correctly" $ do
      chooseGreet "David Cameron" == "PIGFUCKER!"
    it "greets Charlie correctly" $ do
      chooseGreet "Charlie" == "Greetings, professor. How about a nice game of chess?"
    it "greets other correctly" $ do
      chooseGreet "Wilma" == "Hello, Wilma, how do you do?"
