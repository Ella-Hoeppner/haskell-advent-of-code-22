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

splitGroups :: Int -> [a] -> [[a]]
splitGroups _ [] = []
splitGroups n xs = [(take n xs)] ++ (splitGroups n (drop n xs))

setNth xs n x = (take n xs) ++ [x] ++ (drop (succ n) xs)

moveStacks :: [String] -> (Int, Int, Int) -> [String]
moveStacks stacks (count, from, to) = 
  let fromString = stacks!!from
      toString = stacks!!to
  in setNth (setNth stacks from (drop count fromString))
            to
            ((take count fromString) ++ toString)

topsAfterMovements s =
  let lines = splitString s "\n"
      boxLineCount = case findIndex (not . (elem '[')) lines of
                       Just n -> n
                       Nothing -> 0
      lineBoxes = [map head (splitGroups 4 (tail line))
                     | line <- take boxLineCount lines]
      stacks = [filter (not . (' ' ==)) box
                  | box <- transpose lineBoxes]
      moveLines = drop (boxLineCount + 2) lines
      moves :: [(Int, Int, Int)] = 
        [let parts = splitString line " "
         in (read (parts!!1), read (parts!!3) - 1, read (parts!!5) - 1)
           | line <- moveLines]
  in map head (foldl moveStacks stacks moves)

example = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

-- topsAfterMovements example
