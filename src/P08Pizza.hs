module P08Pizza where

import Library

main :: IO ()
main = do
    peeps  <- promptNonNegInt "Number of people: "
    pizzas <- promptNonNegInt "Number of pizzas: "
    spp    <- promptNonNegInt "Slices per pizza: "
    let (slices, leftover) = (spp * pizzas) `divMod` peeps
    putStrLn $ formatPizzas pizzas
             ++ formatSlices slices
             ++ formatLeftovers leftover

backwards :: IO ()
backwards  = do
    peeps        <- promptNonNegInt "Number of people : "
    slicesPerson <- promptNonNegInt "Slices per person: "
    spp          <- promptNonNegInt "Slices per pizza: "
    let pizzasNeeded = (peeps * slicesPerson) `div` spp
    putStrLn $ "For " ++ show peeps ++ " people to have "
             ++ show slicesPerson ++ " slices each and "
             ++ show spp ++ " slices per pizza, you need "
             ++ show pizzasNeeded ++ " pizzas in total"

formatSlices :: Int -> String
formatSlices slices =
    case slices of 
      0 -> " So nobody gets a slice of pizza at all. Boo! And there was"
      1 -> " So each person gets a single slice of pizza, with"
      _ -> " So each person gets " ++ show slices ++ " slices of pizza, with"

formatPizzas :: Int -> String
formatPizzas pizzas =
    case pizzas of 
      0 -> "There was no pizza. Boo!"
      1 -> "There was 1 pizza."
      _ -> "There were " ++ show pizzas ++ " pizzas."

formatLeftovers :: Int -> String
formatLeftovers leftover =
    case leftover of 
      0 -> " no slices left over."
      1 -> " one slice left over."
      _ -> " " ++ show leftover ++ " slices left over."
