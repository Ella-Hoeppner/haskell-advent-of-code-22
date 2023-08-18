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

scoreChoice "X" = 1
scoreChoice "Y" = 2
scoreChoice "Z" = 3

scorePair "A" "Y" = 6
scorePair "B" "Z" = 6
scorePair "C" "X" = 6
scorePair "A" "X" = 3
scorePair "B" "Y" = 3
scorePair "C" "Z" = 3
scorePair _ _ = 0

scoreStrategy strat = 
  sum [let [a,b] = splitString pair " " 
        in (scoreChoice b) + (scorePair a b)
          | pair <- splitString strat "\n", not (null pair)]

-- scoreStrategy "A Y\nB X\nC Z\n"