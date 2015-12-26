module P30MultTblSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P30MultTbl hiding (main)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "multTbl" $ do
    it "returns [(0,0,0)] if called with (-1)" $ do
      multTbl (-1) `shouldBe` [(0,0,0)]
    it "returns [(0,0,0)] if called with 0" $ do
      multTbl 0 `shouldBe` [(0,0,0)]
    it "returns [(0,0,0),(0,1,0),(1,0,0),(1,1,1)] if called with 1" $ do
      multTbl 1 `shouldBe` [(0,0,0),(0,1,0),(1,0,0),(1,1,1)]
  describe "table" $ do
    it "gives a formatted 12 times table when given a multTbl 12" $ do
      (table $ multTbl 12) `shouldBe` "      0   1   2   3   4   5   6   7   8   9  10  11  12 \n  0   0   0   0   0   0   0   0   0   0   0   0   0   0 \n  1   0   1   2   3   4   5   6   7   8   9  10  11  12 \n  2   0   2   4   6   8  10  12  14  16  18  20  22  24 \n  3   0   3   6   9  12  15  18  21  24  27  30  33  36 \n  4   0   4   8  12  16  20  24  28  32  36  40  44  48 \n  5   0   5  10  15  20  25  30  35  40  45  50  55  60 \n  6   0   6  12  18  24  30  36  42  48  54  60  66  72 \n  7   0   7  14  21  28  35  42  49  56  63  70  77  84 \n  8   0   8  16  24  32  40  48  56  64  72  80  88  96 \n  9   0   9  18  27  36  45  54  63  72  81  90  99 108 \n 10   0  10  20  30  40  50  60  70  80  90 100 110 120 \n 11   0  11  22  33  44  55  66  77  88  99 110 121 132 \n 12   0  12  24  36  48  60  72  84  96 108 120 132 144 "
