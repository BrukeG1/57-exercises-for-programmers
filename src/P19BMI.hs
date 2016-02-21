module P19BMI where

import Text.Printf (printf)
import Library
-- TODO: Make a GUI

main :: IO ()
main = do
    u <- promptNonNegNum "Units 1. ft, in/lb; 2: in/lb; 3: kg,cm; default 2: " :: IO Integer
    b <- case u of
        1 -> ftInInput
        3 -> cmInput
        _ -> inInput
    putStrLn $ formatBMI b

data Height = Inches Double | FeetInches Double Double | Centimeters Double
data Weight = Pounds Double | Kilograms Double
data BMIband = Low | Healthy | High

type BMI = Double

toInches :: Height -> Height
toInches (FeetInches f i) = Inches $ (f*12) + i
toInches (Centimeters n) = Inches (n / 2.54)
toInches i = i

toPounds :: Weight -> Weight
toPounds (Kilograms k) = Pounds (k*2.205)
toPounds p = p

formatBMI :: BMI -> String
formatBMI b =
    "Your BMI is: " ++ printf "%.2f" b ++ ". " ++ msg
  where
    band = cmpBMI b
    msg  = case band of
      Low     -> "That is considered low, you should eat some cake or something, skinny."
      Healthy -> "That is considered normal. Well done. You probably have a very boring life."
      High    -> "That is considered high, go for a run and give the chocolate a break, fatty."

inInput :: IO BMI
inInput = do
    h <- promptNonNegNum "Height in inches: "
    w <- promptNonNegNum "Weight in lb: "
    return $ bmi (Inches h) (Pounds w)

ftInInput :: IO BMI
ftInInput = do
    f <- promptNonNegNum "Height (feet part): "
    i <- promptNonNegNum "Height (inches part): "
    w <- promptNonNegNum "Weight in lb: "
    return $ bmi (FeetInches f i) (Pounds w)

cmInput :: IO BMI
cmInput = do
    h <- promptNonNegNum "Height in cm: "
    w <- promptNonNegNum "Weight in kg: "
    return $ bmi (Centimeters h) (Kilograms w)

bmi :: Height -> Weight -> BMI
bmi h w =
    (w' / (h'**2)) * 703
  where
    (Inches h') = toInches h
    (Pounds w') = toPounds w

cmpBMI :: BMI -> BMIband
cmpBMI b | b < 18.5  = Low
         | b > 25    = High
         | otherwise = Healthy
