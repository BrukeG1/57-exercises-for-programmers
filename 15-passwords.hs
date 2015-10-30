import Crypto.BCrypt -- in stack
import Data.ByteString.Char8 (pack)
import System.IO
import qualified Data.Map as M

main :: IO ()
main = do
    user <- promptS "Username: "
    pwd <- promptP "Password: "
    case getPasswordFor user of
      Nothing   -> do if validatePassword (p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu") (p "timing attack resistance?")
                      then putStrLn ""
                      else putStrLn noLoginMsg
      (Just pass) -> do if validatePassword pass (p pwd)
                        then putStrLn "Welcome!"
                        else putStrLn noLoginMsg

-- password == password
uToP = M.fromList $ [ ("charlie",(p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu"))
                    , ("admin",(p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu"))
                    ]

noLoginMsg = "Who the fuck are you?"

getPasswordFor u = M.lookup u uToP

promptS :: String -> IO String
promptS m = do
    putStr m
    getLine

promptP :: String -> IO String
promptP m = do
    putStr m
    hFlush stdout
    hSetEcho stdin False
    p <- getLine
    hSetEcho stdin True
    putStrLn ""
    return p

validPwHash = p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu"
p = pack
makePwHash = hashPasswordUsingPolicy slowerBcryptHashingPolicy . p

-- TODO: make a map of user -> password and validate against that
