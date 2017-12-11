\begin{code}
module Core (
    core, lookup, insert
) where

import Prelude hiding (lookup)
import Instruction.Instruction
import qualified Data.Map.Strict as Map
\end{code}

The core will be implemented as a thin wrapper around Haskell's Map. This is for simplicity in evaluating and updating values, rather than maintaining a large list which will likely contain a large number of DAT 0 (or, empty) values.

The core has a range of allowed values, from 1 to n, where n is whatever size is decided on externally (commonly 8000). All arithmetic and addressing occurs within that valid range, and automatically kept within that range. As such, we need to know what the valid range is.

\begin{code}
data Core = Core Int (Map.Map Int Instruction)
    deriving (Show)
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
