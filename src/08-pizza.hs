main :: IO ()
main = do
    peeps  <- promptF "Number of people: "
    pizzas <- promptF "Number of pizzas: "
    spp    <- promptF "Slices per pizza: "
    let (slices, leftover) = (spp * pizzas) `divMod` peeps
    putStrLn $ formatPizzas pizzas
             ++ formatSlices slices
             ++ formatLeftovers leftover

backwards :: IO ()
backwards  = do
    peeps        <- promptF "Number of people : "
    slicesPerson <- promptF "Slices per person: "
    spp          <- promptF "Slices per pizza: "
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

promptF :: String -> IO Int
promptF m = do
    putStr m
    x <- readLn :: IO Int -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptF m
      else
        return x
