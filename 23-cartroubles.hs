import Data.Char (toUpper)

main :: IO ()
main = nextStep decisionTree

data DecisionTree = Leaf String | Branch String DecisionTree DecisionTree
  deriving (Show)

decisionTree :: DecisionTree
decisionTree =
  Branch "Car silent when you turn key?"
    (Branch "Are battery terminals corroded?"
      (Leaf "Clean terminals and try again")
      (Leaf "Replace cables and try again")
    )
    (Branch "Does car make a clicking noise?"
      (Leaf "Replace battery")
      (Branch "Does car crank up but fail to start?"
        (Leaf "Check spark plug connections")
        (Branch "Does the engine start and then die?"
          (Branch "Does your car have fuel injection?"
            (Leaf "Get it serviced")
            (Leaf "Check that choke is opening and closing")
          )
          (Leaf "Dunno!")
        )
      )
    )

nextStep :: DecisionTree -> IO ()
nextStep (Leaf n) =
    putStrLn n
nextStep (Branch msg left right) = do
  putStrLn msg
  putStr "Yes/No (y/n): "
  yn <- getLine
  case map toUpper yn of
    "Y" -> nextStep left
    "N" -> nextStep right
    _   -> error "bad input (fixme)"

-- # TODO: find rules engine and reimplement this in that
