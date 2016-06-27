{-# LANGUAGE OverloadedStrings #-}
module P48WeatherSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import Data.Fixed (mod')
import P48Weather hiding (main)
import Control.Lens ((^.),(^?))
import Data.Aeson.Lens (_String, _Number, _Integer, key, nth, AsValue)
import Data.Aeson.Types (Value)
import Network.Wreq (get, Response, responseBody)
import Data.Text (unpack)
import P18TempConverter (Temperature (Kelvin), toCelsius, toFarenheit)




main :: IO ()
main = hspec spec

mock = weatherForPlace "London, UK"

spec :: Spec
spec = do
    describe "weatherForPlace" $
      it "gets weather for the correct country" $ do
        w' <-weatherForPlace "Oxford UK"
        (w' ^. responseBody . key "sys" . key "country" . _String) `shouldBe` "GB"
    describe "weatherForPlace" $
      it "gets weather for the correct place name" $ do
        w' <-weatherForPlace "Oxford UK"
        (w' ^. responseBody . key "name" . _String) `shouldBe` "Oxford"
    describe "weatherForPlace" $
      it "gets weather for the correct id" $ do
        w' <-weatherForPlace "Oxford UK"
        (w' ^? responseBody . key "id". _Integer) `shouldBe` Just 2640729
    describe "getTemp" $
      it "gets some reasonably sensible temperature i.e. > 200K (which is cold for anywhere. Even London)." $ do
        m <- mock
        let (Kelvin n) = getTemp m
        n > 200 `shouldBe` True
    describe "getHumidity" $
      it "gets a number in range 0..100 for humidity" $ do
        m <- mock
        let n = getHumidity m
        n >= 0 && n <= 100 `shouldBe` True
    describe "getPressure" $
      it "gets some number for air pressure" $ do
        m <- mock
        let n = getPressure m
        n > 0 `shouldBe` True
    describe "getWindSpeed" $
      it "gets some number for wind speed" $ do
        m <- mock
        let n = getWindSpeed m
        n >= 0 `shouldBe` True
    describe "getWindDirection" $
      it "pends" pending
    describe "getDescription" $
      it "gets some non-empty description string" $ do
        m <- mock
        let n = getDescription m
        unpack n `shouldSatisfy` (not . null)
    describe "getSunrise" $
      it "gets some non-empty sunrise time string" $ do
        m <- mock
        let n = getSunrise m
        n `shouldSatisfy` (not . null)
    describe "getSunset" $
      it "gets some non-empty sunset time string" $ do
        m <- mock
        let n = getSunset m
        n `shouldSatisfy` (not . null)
    describe "degToDirection" $
      prop "Direction is always the same mod 360" $
        \x -> degToDirection x == degToDirection (x `mod'` 360.0)
    describe "unixToDateString" $
      it "pends" pending

