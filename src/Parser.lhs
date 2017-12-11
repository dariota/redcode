\begin{code}
module Parser
    ( readProgram
    ) where

import Instruction.Instruction
import Data.Char (isSpace)
\end{code}

We want to be able to convert a string (representing a full file containg a redcode program) into a list of instructions. This can then be used once for each program to run in the MARS simulator.

\begin{code}
readProgram :: String -> [Instruction]
readProgram input = map (read) $ stripEmpty $ map stripComment $ lines input
\end{code}

We now implement the pre-processing functions to remove comments and empty lines.

\begin{code}
stripComment :: String -> String
stripComment (';':xs) = []
stripComment (x:xs)   = x : stripComment xs
stripComment []       = []

stripEmpty :: [String] -> [String]
stripEmpty = filter (\x -> (dropWhile isSpace x) /= [])
\end{code}

Thanks to the amount of offloading we can do to the instruction's read implementation, the parser is extremely simple, and is already complete.
