module PwStrength25 where
import Data.Char

main :: IO ()
main = do
    putStr "Enter password: "
    p <- getLine
    case passwordStrength p of
      VeryWeak   -> putStrLn $ "'" ++ p ++ "' is a very weak password. Loser"
      Weak       -> putStrLn $ "'" ++ p ++ "' is a weak password. Lame"
      Average    -> putStrLn $ "'" ++ p ++ "' is an average password. Try harder"
      Strong     -> putStrLn $ "'" ++ p ++ "' is a strong password. Nice"
      VeryStrong -> putStrLn $ "'" ++ p ++ "' is a very strong password. You are scary"

-- because 8 is going to need to change
strongPasswordMinLength :: Int
strongPasswordMinLength = 8

data PasswordStrength = VeryWeak | Weak | Average | Strong | VeryStrong deriving (Show, Ord, Eq)

passwordStrength :: String -> PasswordStrength
passwordStrength ps | length ps >= l && manyLetters ps && manyNumbers ps && manySpecials ps
                        = VeryStrong
                    | length ps >= l && hasNumbers ps && manyLetters ps
                        = Strong
                    | length ps < l && allLetters ps
                        = Weak
                    | length ps < l && allNumbers ps
                        = VeryWeak
                    | otherwise
                        = Average
  where
    l = strongPasswordMinLength

hasLetters, hasNumbers, hasSpecials, allLetters, allNumbers,
  manyLetters, manyNumbers, manySpecials :: String -> Bool

hasLetters   = any isLetter
manyLetters  = many isLetter
allLetters   = all isLetter
hasNumbers   = any isNumber
manyNumbers  = many isNumber
allNumbers   = all isNumber
hasSpecials  = any (\x -> isPunctuation x || isSymbol x)
manySpecials = many (\x -> isPunctuation x || isSymbol x)

many :: (a -> Bool) -> [a] -> Bool
many f = (1<) . length . filter f
