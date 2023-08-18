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

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = [(take n xs)] ++ (partition n (drop n xs))

findBadgeType [s1, s2, s3] =
  head [c | c <- s1, elem c s2, elem c s3]

typePriority c 
  | elem c ['a'..'z'] = 1 + (ord c) - (ord 'a')
  | otherwise         = 27 + (ord c) - (ord 'A')

badgePrioritySum s =
  sum [(typePriority . findBadgeType) group 
         | group <- partition 3 (splitString s "\n")]

-- badgePrioritySum "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
