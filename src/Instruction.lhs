\begin{code}
module Instruction
    ( Instruction, defaultIns, aField, bField,
      Value
    ) where

import Data.Char (isDigit, toLower, isSpace)
\end{code}

Instructions are composed of an opcode and up to two fields, A and B, each containing a value. These values have their own semantics. In instructions where only one field is used, the other field is zero.

We define convenience functions, aField and bField, which return instructions' fields or the default value.

\begin{code}
data Instruction = Dat Value
                 | Mov Value Value
                 | Add Value Value
                 | Sub Value Value
                 | Jmp Value
                 | Jmz Value Value
                 | Jmn Value Value
                 | Djn Value Value
                 | Cmp Value Value
                 | Spl Value
                 deriving (Show)

defaultIns :: Instruction
defaultIns = Dat (Direct 0)

aField :: Instruction -> Value
aField ins = case ins of
                Dat _   -> defaultVal
                Mov v _ -> v
                Add v _ -> v
                Sub v _ -> v
                Jmp v   -> v
                Jmz v _ -> v
                Jmn v _ -> v
                Djn v _ -> v
                Cmp v _ -> v
                Spl v   -> v

bField :: Instruction -> Value
bField ins = case ins of
                Dat v   -> v 
                Mov _ v -> v
                Add _ v -> v
                Sub _ v -> v
                Jmp _   -> defaultVal
                Jmz _ v -> v
                Jmn _ v -> v
                Djn _ v -> v
                Cmp _ v -> v
                Spl _   -> defaultVal
\end{code}

Values are comprised of two parts, the value and the addressing mode. The addressing mode may be ignored depending on the instruction.

\begin{code}
data Value = Direct Int
           | Indirect Int
           | Immediate Int
           | Autodecrement Int
           deriving (Show)

defaultVal :: Value
defaultVal = Direct 0
\end{code}

We want to be able to parse redcode programs, so we'll need a custom Read instance for Instruction, which will require one for Value as well. Since Value is the simpler one (and we'll need it to parse the Instruction anyway), we'll start with that. (with thanks to this StackOverflow answer for showing a clear and simple implementation of Read https://stackoverflow.com/a/14006938/6519610)

\begin{code}
instance Read Value where
    readsPrec _ input = 
        let (mode, rest1) = span (flip elem "$@#<") input
            (num, rest2) = span isDigit rest1
            value = read num :: Int
            addressing = case mode of
                         ""        -> Direct
                         "$"       -> Direct
                         "@"       -> Indirect
                         "#"       -> Immediate
                         "<"       -> Autodecrement
            in [(addressing value, rest2)]
\end{code}

Now we just need to do the same for the Instruction. This part is slightly more complicated since we need to check if the constructor needs one or two values. We split the parsing into two sub functions - one which parses an instruction with a single value and one which parses instructions with two values.

We will ignore the existence of comments, assume lines are not blank, and assume that lines do not contain only comments. These cases will all be handled by the parser/pre-processor.

\begin{code}
instance Read Instruction where
    readsPrec _ i = parseWith op rest2
      where input = map toLower i
            (op, rest1) = span (not . isSpace) input
            rest2 = dropWhile isSpace rest1
            parseWith = if op `elem` ["dat", "jmp", "spl"] then readSingle else readDouble

readSingle :: String -> String -> [(Instruction, String)]
readSingle op input = 
    let constructor = case op of
                        "dat" -> Dat
                        "jmp" -> Jmp
                        "spl" -> Spl
        [(value, rest1)] = readsPrec 0 input :: [(Value, String)]
    in [(constructor value, rest1)]

readDouble :: String -> String -> [(Instruction, String)]
readDouble op input =
    let constructor = case op of
                        "mov" -> Mov
                        "add" -> Add
                        "sub" -> Sub
                        "jmz" -> Jmz
                        "jmn" -> Jmn
                        "djn" -> Djn
                        "cmp" -> Cmp
                        otherwise -> error $ "Invalid opcode " ++ op
        [(first, rest1)] = readsPrec 0 input :: [(Value, String)]
        rest2 = dropWhile isSpace $ dropIf ',' $ dropWhile isSpace rest1
        [(second, rest3)] = readsPrec 0 rest2 :: [(Value, String)]
    in [(constructor first second, rest3)]
    where dropIf c xs = if (head xs == c) then drop 1 xs else error $ "Expected " ++ [c] ++ " between values but got " ++ [head xs]
\end{code}

This concludes the definition of Instructions - actually executing them is the remit of the program/execution units.
