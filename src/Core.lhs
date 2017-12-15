\begin{code}
module Core (
    Mars, Core, core, lookup, insert, positionPrograms, display
) where

import Prelude hiding (lookup)
import Instruction.Instruction
import Control.Monad.Trans.State
import Control.Monad.Identity
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map.Strict as Map
\end{code}

The core will be implemented as a thin wrapper around Haskell's Map. This is for simplicity in evaluating and updating values, rather than maintaining a large list which will likely contain a large number of DAT 0 (or, empty) values.

The core has a range of allowed values, from 1 to n, where n is whatever size is decided on externally (commonly 8000). All arithmetic and addressing occurs within that valid range, and automatically kept within that range. As such, we need to know what the valid range is.

\begin{code}
data Mars = Mars {size :: Int, memory :: (Map.Map Int Instruction)}
    deriving (Show)

type Core a = StateT Mars Identity a
\end{code}

We now create a wrapper function that creates our map, initially empty, as well as wrapper functions on its insert and lookup that ensure the key is valid before use and return a DAT 0 if the requested location is empty.

\begin{code}
core :: Int -> Mars
core size = Mars size Map.empty

adjust :: Int -> Core Int
adjust i = do
    mars <- get
    let max = size mars
    let moded = i `mod` max
    pure $ if moded <= 0 then moded + max else moded

lookup :: Int -> Core Instruction
lookup pos = do
    mars <- get
    ind <- adjust pos
    pure $ Map.findWithDefault defaultIns ind (memory mars)

insert :: Int -> Instruction -> Core ()
insert pos ins = do
    mars <- get
    ind <- adjust pos
    let newMap = Map.insert ind ins (memory mars)
    put $ Mars (size mars) newMap
\end{code}

We'll want to initialise the core with a number of programs. For simplicity, we'll try to space these somewhat eventually, allowing each one an equal space allocation which it'll be placed in the middle of.

\begin{code}
positionPrograms :: Monad m => [[Instruction]] -> Core (m [Int])
positionPrograms [] = pure $ return []
positionPrograms is = do
    mars <- get
    let spaceAlloc = size mars `div` progCount
    let remainingSpace = size mars - (foldl1 (+) progLengths)
    progPositions <- findPositions progLengths 1 spaceAlloc
    insertPrograms $ zip progPositions is
    if remainingSpace < 0 then
        pure $ fail "Insufficient space for programs"
    else
        pure $ return progPositions
    where progCount = length is
          progLengths = map length is

insertPrograms :: [(Int, [Instruction])] -> Core ()
insertPrograms [] = pure ()
insertPrograms ((pos, ins):xs) = do
    insertAt pos ins
    insertPrograms xs
\end{code}

In the interest of fairness, we'll give each program the same amount of space (also meaning a simple program can't dominate a huge area of the core due to the size restriction) and position it right in the centre of its space allowance.

\begin{code}
findPositions :: Monad m => [Int] -> Int -> Int -> m [Int]
findPositions [] _ _ = return []
findPositions (x:xs) start space = do
    let fromStart = start + (space - x) `div` 2
    tailPart <- findPositions xs (start + space) space
    if space - x < 0 then
        fail "Insufficient space for a program"
    else
        return $ fromStart : tailPart
\end{code}

Lastly we define a helper function to insert an entire program at a specific point in the core.

\begin{code}
insertAt :: Int -> [Instruction] -> Core ()
insertAt _ [] = pure ()
insertAt i (x:xs) = do
    insert i x
    insertAt (i + 1) xs
\end{code}

For the purposes of a neat interface, we'll want to print the modified contents of the core in a nice way.

\begin{code}
display :: Mars -> String
display (Mars size map) = foldl (\acc (pos, ins) -> acc ++ (align pos size) ++ " " ++ (show ins) ++ "\n") "" vals
    where vals = Map.assocs map

align :: Int -> Int -> String
align i size = (replicate diff ' ') ++ iS
    where iS = show i
          sizeS = show size
          diff = (length sizeS) - (length iS)
\end{code}
