import Data.List

splitStringRec :: [String] -> String -> String -> [String]
splitStringRec splitStrings "" _ = splitStrings
splitStringRec splitStrings s splitter = 
  if splitter == (take (length splitter) s)
    then splitStringRec (splitStrings ++ [""]) 
                        (drop (length splitter) s)
                        splitter
    else splitStringRec ((init splitStrings) ++ [(last splitStrings) ++ [(head s)]])
                        (tail s)
                        splitter

splitString :: String -> String -> [String]
splitString = splitStringRec [""]

countCalories :: String -> [Int]
countCalories calorieString = 
  [sum [read calorieString :: Int | calorieString <- splitString items "\n", not (null calorieString)] | items <- splitString calorieString "\n\n"]

top3Calories :: String -> Int
top3Calories = sum . (take 3) . reverse . sort . countCalories

--top3Calories "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n"
