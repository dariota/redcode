\begin{code}
module Core (
    Core, core, lookup, insert, positionPrograms
) where

import Prelude hiding (lookup)
import Instruction.Instruction
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map.Strict as Map
\end{code}

The core will be implemented as a thin wrapper around Haskell's Map. This is for simplicity in evaluating and updating values, rather than maintaining a large list which will likely contain a large number of DAT 0 (or, empty) values.

The core has a range of allowed values, from 1 to n, where n is whatever size is decided on externally (commonly 8000). All arithmetic and addressing occurs within that valid range, and automatically kept within that range. As such, we need to know what the valid range is.

\begin{code}
data Core = Core Int (Map.Map Int Instruction)
    deriving (Show)

size :: Core -> Int
size (Core i _) = i
\end{code}

We now create a wrapper function that creates our map, initially empty, as well as wrapper functions on its insert and lookup that ensure the key is valid before use and return a DAT 0 if the requested location is empty.

\begin{code}
core :: Int -> Core
core size = Core size Map.empty

adjust :: Int -> Int -> Int
adjust size i = bounded
    where moded = i `mod` size
          bounded = if moded < 0 then moded + size else moded

lookup :: Core -> Int -> Instruction
lookup (Core size map) pos = Map.findWithDefault defaultIns ind map
    where ind = adjust size pos

insert :: Core -> Int -> Instruction -> Core
insert (Core size map) pos ins = Core size newMap
    where newMap = Map.insert (adjust size pos) ins map 
\end{code}

We'll want to initialise the core with a number of programs. For simplicity, we'll try to space these somewhat eventually, allowing each one an equal space allocation which it'll be placed in the middle of.

\begin{code}
positionPrograms :: Core -> [[Instruction]] -> Maybe ([Int], Core)
positionPrograms c [] = Just ([], c)
positionPrograms c is = if remainingSpace < 0 || isNothing progPositions then Nothing else Just (fromJust progPositions, newCore)
    where progCount = length is
          spaceAlloc = size c `div` progCount
          progLengths = map length is
          remainingSpace = size c - (foldl1 (+) progLengths)
          progPositions = findPositions progLengths 1 spaceAlloc
          newCore = insertPrograms c $ zip (fromJust progPositions) is

insertPrograms :: Core -> [(Int, [Instruction])] -> Core
insertPrograms = foldl (\c' (pos, ins) -> insertAt c' pos ins)
\end{code}

In the interest of fairness, we'll give each program the same amount of space (also meaning a simple program can't dominate a huge area of the core due to the size restriction) and position it right in the centre of its space allowance.

\begin{code}
findPositions :: [Int] -> Int -> Int -> Maybe [Int]
findPositions [] _ _ = Just []
findPositions (x:xs) start space = res
    where fromStart = start + (space - x) `div` 2
          tailPart = findPositions xs (start + space) space
          res = if space - x < 0 then Nothing else combine fromStart tailPart

combine :: Int -> Maybe [Int] -> Maybe [Int]
combine _ Nothing = Nothing
combine x (Just xs) = Just $ x : xs
\end{code}

Lastly we define a helper function to insert an entire program at a specific point in the core.

\begin{code}
insertAt :: Core -> Int -> [Instruction] -> Core
insertAt c _ [] = c
insertAt c i (x:xs) = c'
    where c'' = insert c i x
          c' = insertAt c'' (i+1) xs
\end{code}
