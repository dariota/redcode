\begin{code}
module Instruction.Resolver (
    resolve
) where

import Prelude hiding (lookup)
import Instruction.Instruction
import Core
\end{code}

We'll want to be able to resolve a value to the actual value it refers to given the core, and also update the core if needed (for example, for a auto-decrement).

\begin{code}
resolve :: Int -> Value -> Core Int
resolve currPos (Direct val) = pure $ currPos + val
resolve currPos (Indirect val) = resolveIndirect currPos val
resolve currPos (Immediate val) = pure val
resolve currPos (Autodecrement val) = do
    let refPos = currPos + val
    referenced <- lookup refPos
    let refVal = valuePart $ bField referenced
    let newRef = withB referenced (refVal - 1)
    insert refPos newRef
    resolveIndirect currPos val

resolveIndirect :: Int -> Int -> Core Int
resolveIndirect currPos val = do
    referenced <- lookup (currPos + val)
    let refValue = valuePart $ bField referenced
    pure $ currPos + refValue
\end{code}
