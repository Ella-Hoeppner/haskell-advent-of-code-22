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

moveStacks stacks (from, to) = 
  let fromHead : fromTail = stacks!!from
  in setNth (setNth stacks from fromTail)
            to
            (fromHead:stacks!!to)

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
      moveInstructions :: [[Int]] = 
        [let parts = splitString line " "
         in map read [parts!!1, parts!!3, parts!!5]
           | line <- moveLines]
      moves = concat [take times (repeat (from - 1, to - 1)) 
                        | [times, from, to] <- moveInstructions]
  in map head (foldl moveStacks stacks moves)

example = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

-- topsAfterMovements example
