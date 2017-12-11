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
resolve :: Int -> Value -> Core -> (Int, Core)
resolve i (Direct a) c = (i + a, c)
resolve i (Indirect a) c = resolveImmediate i a c
resolve i (Immediate a) c = (a, c)
resolve i (Autodecrement a) c = resolveImmediate i (valuePart $ bField targeted') c'
    where targeted = lookup c (i + a)
          targetValue = valuePart $ bField targeted
          targeted' = withB targeted (targetValue - 1)
          c' = insert c (i + a) targeted'

resolveImmediate :: Int -> Int -> Core -> (Int, Core)
resolveImmediate i a c = (i + refValue, c)
    where referenced = lookup c (i + a)
          refValue = valuePart $ bField referenced
\end{code}
