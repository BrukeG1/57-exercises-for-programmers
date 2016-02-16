module P15Passwords where

import Crypto.BCrypt
import Data.ByteString.Char8 (pack,ByteString)
import qualified Data.Map as M
import Library

main :: IO ()
main = do
    user <- promptS "Username: "
    pwd <- promptP "Password: "
    putStrLn $ login user pwd

login :: String -> String -> String
login user pwd =
    case getPasswordFor user of
      Nothing     -> do if validatePassword (p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu") (p "timing attack resistance?")
                        then ""
                        else noLoginMsg
      (Just pass) -> do if validatePassword pass (p pwd)
                        then loginMsg
                        else noLoginMsg

-- password == password
uToP :: M.Map String (ByteString)
uToP = M.fromList $ [ ("charlie",(p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu"))
                    , ("admin",(p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu"))
                    ]

noLoginMsg :: String
noLoginMsg = "Who the fuck are you?"

loginMsg :: String
loginMsg = "Welcome!"

getPasswordFor :: String -> Maybe ByteString
getPasswordFor u = M.lookup u uToP

validPwHash :: ByteString
validPwHash = p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu"

p :: String -> ByteString
p = pack

makePwHash :: String -> IO (Maybe ByteString)
makePwHash = hashPasswordUsingPolicy slowerBcryptHashingPolicy . p
