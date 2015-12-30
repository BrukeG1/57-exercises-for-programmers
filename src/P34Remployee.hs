module P34Remployee where

type Employee = String

employeeFile :: FilePath
employeeFile = "employeeList.txt"

fileEmployees :: IO [Employee]
fileEmployees = do
    es <- readFile employeeFile
    return $ lines es

arrEmployees :: [Employee]
arrEmployees =
    [ "Danbert Nobacon"
    , "Jimmy 'The Monkey' Winkelborough"
    , "Madame Phylis Dernier LaTouche"
    , "Barry White"
    , "Alfredo Juan Lopez"
    , "Dilbert"
    , "O"
    ]

main :: IO () 
main = do
    employees <- fileEmployees
    putStrLn $ showNumEmployees employees
    putStrLn $ showEmployees employees
    putStr "Which you wanna remove? "
    tr <- getLine
    let newEmp = remp tr employees
    if length newEmp == length employees
      then putStrLn $ "No employee removed, " ++ tr ++ " was not found"
      else do
        putStrLn $ showNumEmployees newEmp
        putStrLn $ showEmployees newEmp
        writeFile employeeFile $ showEmployees newEmp

showEmployees :: [Employee] -> String
showEmployees =  unlines

showNumEmployees :: [Employee] -> String
showNumEmployees es = "There are " ++ show (length es) ++ " employees!\n"

remp :: Employee -> [Employee] -> [Employee]
remp _ [] = []
remp e (x:xs) | e == x = remp e xs
              | otherwise = x : remp e xs
