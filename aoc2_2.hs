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

scoreChoice "X" = 0
scoreChoice "Y" = 3
scoreChoice "Z" = 6

scorePair "A" "X" = 3
scorePair "A" "Y" = 1
scorePair "A" "Z" = 2
scorePair "B" "X" = 1
scorePair "B" "Y" = 2
scorePair "B" "Z" = 3
scorePair "C" "X" = 2
scorePair "C" "Y" = 3
scorePair "C" "Z" = 1

scoreStrategy strat = 
  sum [let [a,b] = splitString pair " " 
        in (scoreChoice b) + (scorePair a b)
          | pair <- splitString strat "\n", not (null pair)]

-- scoreStrategy "A Y\nB X\nC Z\n"