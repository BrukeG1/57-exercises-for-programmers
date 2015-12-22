import Data.Map (fromList, (!), member, Map)
import Data.Char (toLower)
import Text.Printf
import Control.Exception (catch, IOException)

main :: IO () 
main = do
    amt <- promptN "What is the order amount? "
    putStr "What state are you in? "
    state <- getLine
    putStr "What county are you in? "
    county <- getLine
    let taxAmt = getTaxAmt state county amt
    let taxAmt' = getTaxAmt' state county amt
    let (taxMsg,total) = if taxAmt == 0 then ("",amt) else (printf "Tax:        %16.2f\n" taxAmt,amt+taxAmt)
    let (taxMsg',total') = if taxAmt' == 0 then ("",amt) else (printf "Tax(alt):   %16.2f\n" taxAmt', amt+taxAmt')
    putStrLn $ taxMsg ++ printf "Total:      %16.2f" total
    putStrLn $ taxMsg' ++ printf "Total(alt): %16.2f" total'

-- Given a string, give back a lowercase version of it
lc :: String -> String
lc = map toLower

-- Prompt for a number. Make sure it is positive and valid.
promptN :: (Num a, Ord a, Read a) => String -> IO a
promptN m = do
    putStr m
    x <- readLn `catch` except
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptN m
      else
        return x
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptN m

-- Original implementation with pattern matching. Nicer.
-- Given a state, county and amount pass back the correct tax amount
getTaxAmt :: String -> String -> Double -> Double
getTaxAmt state county amt =
   case (lc state,lc county) of
     ("wisconsin","eau clair") -> amt * 0.005
     ("wn","eau clair")        -> amt * 0.005
     ("wisconsin","dunn")      -> amt * 0.004
     ("wn","dunn")             -> amt * 0.004
     ("illinois",_)            -> amt * 0.08
     ("il",_)                  -> amt * 0.08
     ("oxon",_)                -> amt * 0.2
     ("oxfordshire",_)         -> amt * 0.2
     _                         -> 0

-- Doing this with data structures is fugli, but it was one of the challenges
data TaxLookup = Rate Double | Subregions (Map String TaxLookup)

taxLookup :: TaxLookup
taxLookup =
    Subregions (fromList [ ( "wisconsin"
                           , Subregions (fromList [ ("eau clair", Rate 0.005)
                                                , ("dunn", Rate 0.004)
                                                ])
                           )
                         , ( "wn"
                           , Subregions (fromList [ ("eau clair", Rate 0.005)
                                                  , ("dunn", Rate 0.004)
                                                  ])
                           )
                         , ( "illinois", Rate 0.08)
                         , ( "il", Rate 0.08)
                         , ( "oxon", Rate 0.2)
                         , ( "oxfordshire", Rate 0.2)
                         ]
               )

-- Given a string name of a region, and a lookup; get back either a
-- load of subregions or the rate for the region or 0 rate if the region
-- cannot be looked up
getTaxRateFor :: String -> TaxLookup -> TaxLookup
getTaxRateFor _ r@(Rate _) = r
getTaxRateFor region (Subregions subregions) =
    if member (lc region) subregions
      then subregions ! lc region
      else Rate 0

-- Given a state, county and amount pass back the correct tax amount
getTaxAmt' :: String -> String -> Double -> Double
getTaxAmt' state county amt =
    case getTaxRateFor state taxLookup of
      (Rate 0) -> 0
      (Rate n) -> amt * n
      counties@(Subregions _) ->
          case getTaxRateFor county counties of
            (Rate 0) -> 0
            (Rate n) -> amt * n
            _        -> 0
