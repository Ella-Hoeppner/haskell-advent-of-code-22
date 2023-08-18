import Data.Char (ord)

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

rangesOverlap [[min1, max1], [min2, max2]] =
  max1 >= min2 && min1 <= max2

elfRangesStringInvalid s = 
  rangesOverlap
    [[read bound :: Int | bound <- splitString rangeString "-"]
       | rangeString <- splitString s ","]

invalidElfRangesCount s = 
  sum [1 | line <- splitString s "\n", elfRangesStringInvalid line]

-- invalidElfRangesCount "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"