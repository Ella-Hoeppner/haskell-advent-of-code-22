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

findMisplacedType s =
  let l = length s
      comp1 = take (div l 2) s
      comp2 = drop (div l 2) s
  in head [c | c <- comp1, elem c comp2]

typePriority c 
  | elem c ['a'..'z'] = 1 + (ord c) - (ord 'a')
  | otherwise         = 27 + (ord c) - (ord 'A')

prioritySum s = 
  sum [(typePriority . findMisplacedType) line
         | line <- splitString s "\n", not (null line)]

-- prioritySum "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n"