module P24AnagramSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))
import P24Anagram (isAnagram,mergeSort)
import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isAnagram" $ do
      it "identifies a word as an anagram of itself" $ do
            isAnagram "foppishly" "foppishly"
      it "identifies a backwards word as an anagram of itself" $ do
            isAnagram "flapping" (reverse "flapping")
      it "identifies anagrams" $ do
            isAnagram "note" "tone" `shouldBe` True
      it "ignores spaces when identifying anagrams" $ do
            isAnagram "note" "ne to" `shouldBe` True
      it "identifies non-anagrams" $ do
            isAnagram "none" "tone" `shouldBe` False
      it "ignores spaces when identifying non-anagrams" $ do
            isAnagram "no ne" "t on e" `shouldBe` False
      prop "has the property that reversing arguments gives same result" $
          \xs ys -> isAnagram xs ys == isAnagram ys xs
  describe "mergeSort" $ do
      it "identifies the empty list as the sorted version of itself when called with (<)" $ do
          (mergeSort (<) [] :: [Int]) `shouldBe` []
      it "identifies the empty list as the sorted version of itself when called with (>)" $ do
          (mergeSort (>) [] :: [Int]) `shouldBe` []
      it "sorts a 2 item list when called with (<)" $ do
          mergeSort (<) [2,1] `shouldBe` [1,2]
      it "sorts a 2 item list when called with (>)" $ do
          mergeSort (>) [2,1] `shouldBe` [2,1]
      prop "called with (<) and a list of Ints gives the same result as standard sort from Data.List" $
          \xs -> mergeSort (<) (xs :: [Int]) == sort xs
      prop "called with (>) and a list of Ints gives the same result as reverse of standard sort from Data.List" $
          \xs -> mergeSort (>) (xs :: [Int]) == (reverse $ sort xs)
      prop "is idempotent when called with (<) and a list of Ints" $
          \xs -> mergeSort (<) (xs :: [Int]) == mergeSort (<) (mergeSort (<) xs)
      prop "is idempotent when called with (>) and a list of Ints" $
          \xs -> mergeSort (>) (xs :: [Int]) == mergeSort (>) (mergeSort (>) xs)
      prop "is idempotent when called with (<) and a String" $
          \xs -> mergeSort (<) (xs :: String) == mergeSort (<) (mergeSort (<) xs)
      prop "head of mergeSorted list when called with (<) is minimum item of non-empty list" $
          \xs -> not (null xs) ==> (head $ mergeSort (<) (xs :: String)) == minimum xs
      prop "last of mergeSortedList when called with (<) is maximum item of non-empty list" $
          \xs -> not (null xs) ==> (last $ mergeSort (<) (xs :: String)) == maximum xs
