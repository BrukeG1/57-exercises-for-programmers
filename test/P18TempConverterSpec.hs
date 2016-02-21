module P18TempConverterSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))
import P18TempConverter hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toCelsius" $ do
      prop "is idempotent on Celsius inputs" $
        \x -> toCelsius (Celsius x) == (toCelsius $ toCelsius (Celsius x))
      prop "is idempotent on Farenheit inputs" $
        \x -> toCelsius (Farenheit x) == (toCelsius $ toCelsius (Farenheit x))
      prop "is idempotent on Kelvin inputs" $
        \x -> toCelsius (Kelvin x) == (toCelsius $ toCelsius (Kelvin x))
      prop "has property that a Celsius temp is equal to itself" $
        \x -> Celsius x == Celsius x
      prop "has property that addition of 2 Celsius figures is same as constructing C after adding them" $
        \x y -> Celsius (x+y) == Celsius x + Celsius y
      prop "has the property that C x - C y is equal to C x-y" $
        \x y -> Celsius (x-y) == Celsius x - Celsius y
      prop "has the property that C x * C y is equal to C x*y" $
        \x y -> Celsius (x*y) == Celsius x * Celsius y
      prop "has the property that fromInteger x = C x" $
        \x -> fromInteger x == Celsius (fromInteger x)
      prop "has the property that abs (C x) = C (abs x)" $
        \x -> abs (Celsius x) == Celsius (abs x)
      prop "has the property that signum (C x) = C (signum x)" $
        \x -> signum (Celsius x) == Celsius (signum x)
      prop "has the property that showing C x gives the string x°C" $
        \x -> show (Celsius x) `shouldBe` show x ++ "°C"
  describe "toFarenheit" $ do
      prop "is idempotent on Farenheit inputs" $
        \x -> toFarenheit (Farenheit x) == (toFarenheit $ toFarenheit (Farenheit x))
      prop "is idempotent on Farenheit inputs" $
        \x -> toFarenheit (Farenheit x) == (toFarenheit $ toFarenheit (Farenheit x))
      prop "is idempotent on Kelvin inputs" $
        \x -> toFarenheit (Kelvin x) == (toFarenheit $ toFarenheit (Kelvin x))
      prop "has property that a Farenheit temp is equal to itself" $
        \x -> Farenheit x == Farenheit x
  describe "toKelvin" $ do
      prop "is idempotent on Kelvin inputs" $
        \x -> toKelvin (Kelvin x) == (toKelvin $ toKelvin (Kelvin x))
      prop "is idempotent on Farenheit inputs" $
        \x -> toKelvin (Farenheit x) == (toKelvin $ toKelvin (Farenheit x))
      prop "is idempotent on Kelvin inputs" $
        \x -> toKelvin (Kelvin x) == (toKelvin $ toKelvin (Kelvin x))
      prop "has property that a Kelvin temp is equal to itself" $
        \x -> Kelvin x == Kelvin x
