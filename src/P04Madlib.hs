module P04Madlib where

import Control.Monad
import Library

main :: IO ()
main = do
    n <- prompt "Noun: "
    v <- prompt "Verb: "
    adj <- prompt "Adjective: "
    adv <- prompt "Adverb: "
    mkStory v n adj adv >>= putStrLn

mkStory :: String -> String -> String -> String -> IO String
mkStory v n adj adv =
    liftM head $ shuffle (stories v n adj adv)

prompt :: String -> IO String
prompt s = do
    putStr s
    getLine

stories :: String -> String -> String -> String -> [String]
stories v n adj adv =
    [ "Do you " ++ v ++ " your " ++ adj ++ " " ++ n ++ " " ++ adv ++ "? That's hilarious!"
    , "Oh flip! Why do you " ++ v ++ " this " ++ adj ++ " " ++ n ++ "? Weirdo."
    , "How does " ++ v ++ " a " ++ adj ++ " " ++ n ++ " work?"
    ]

