import Text.Printf (printf)
-- TODO: Make a GUI

main :: IO ()
main = do
    u <- promptN "Units 1. ft, in/lb; 2: in/lb; 3: kg,cm; default 2: " :: IO Integer
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
    h <- promptN "Height in inches: "
    w <- promptN "Weight in lb: "
    return $ bmi (Inches h) (Pounds w)

ftInInput :: IO BMI
ftInInput = do
    f <- promptN "Height (feet part): "
    i <- promptN "Height (inches part): "
    w <- promptN "Weight in lb: "
    return $ bmi (FeetInches f i) (Pounds w)

cmInput :: IO BMI
cmInput = do
    h <- promptN "Height in cm: "
    w <- promptN "Weight in kg: "
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

promptN :: (Num a, Ord a, Read a) => String -> IO a
promptN m = do
    putStr m
    x <- readLn -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptN m
      else
        return x
