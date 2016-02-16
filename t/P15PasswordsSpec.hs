module P15PasswordsSpec (main,spec) where

import Test.Hspec
import P15Passwords hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getPasswordFor" $ do
    it "gives nothing for a non-existing user" $
      getPasswordFor "willa" `shouldBe` Nothing
    it "gives passwd hash for an existing user" $
      getPasswordFor "charlie" `shouldBe` (Just $ p "$2y$14$WhC4S8aJ0tZO0W7CL7pL4u.cegZHOSsZsFj298Fpg7LI/63Xxa5Hu")
  describe "login" $ do
    it "logs in a valid user with a correct password" $
      login "admin" "password" `shouldBe` "Welcome!"
    it "does not log in in a valid user with an incorrect password" $
      login "admin" "passwrd" `shouldBe` "Who the fuck are you?"
    it "does not log in in an invalid user with an incorrect password" $
      login "admn" "passwrd" `shouldBe` "Who the fuck are you?"
    it "does not log in in an invalid user with a correct password" $
      login "admn" "password" `shouldBe` "Who the fuck are you?"
