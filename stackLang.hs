module StackLang where

import Prelude hiding (Num)


--
-- * Syntax of StackLang
--

-- Grammar for StackLang:
-- 
--    num ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= num                         push a number on the stack
--         |  bool                        push a boolean on the stack
--         |  `add`                       add the top two numbers the stack
--         |  `equ`                       check whether the top two elements are equal
--         |  `if` prog `else` prog `end` if the value on the top is true
--                                        then run the first program, else run the second


-- 1. Encode the above grammar as a set of Haskell data types

-- data Prog = Seq Cmd Prog | End

type Prog = [Cmd]

data Cmd
   = PushI Int
   | PushB Bool
   | Add
   | Equ
   | IfElse Prog Prog
  deriving (Eq,Show)


-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 add 5 equ
--
ex1 :: Prog
ex1 = [PushI 3, PushI 4, Add, PushI 5, Equ]


-- 3. Write a StackLang program that:
--     * checks whether 3 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax as a Haskell value.
--
--    3 4 equ if 5 6 add else false end
--
ex2 :: Prog
ex2 = [PushI 3, PushI 4, Equ, IfElse [PushI 5, PushI 6, Add] [PushB False]]


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
genAdd2 :: Int -> Int -> Prog
genAdd2 x y = [PushI x, PushI y, Add, Add]


-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
genSum :: [Int] -> Prog
genSum []     = [PushI 0]
genSum (i:is) = genSum is ++ [PushI i, Add]
-- genSum (i:is) = [PushI i] ++ genSum is ++ [Add]



--
-- * Semantics of StackLang (now!)
--


-- 6. Identify/define a semantics domain for Cmd and for Prog.
--    Things we need:
--      * stack
--        * int
--        * bool
--      * error
--        * type error
--        * underflow

type Stack = [Either Int Bool]

type Domain = Stack -> Maybe Stack


-- 7. Define the semantics of a StackLang command (ignore If at first).
cmd :: Cmd -> Domain
cmd (PushI i)    = \s -> Just (Left i : s)
cmd (PushB b)    = \s -> Just (Right b : s)
cmd Add          = \s -> case s of
                           (Left i : Left j : s') -> Just (Left (i+j) : s')
                           _ -> Nothing
cmd Equ          = \s -> case s of
                           (Left i : Left j : s') -> Just (Right (i==j) : s')
                           (Right b : Right c : s') -> Just (Right (b==c) : s')
                           _ -> Nothing
cmd (IfElse t e) = \s -> case s of
                           (Right True : s')  -> prog t s'
                           (Right False : s') -> prog e s'
                           _ -> Nothing

-- 8. Define the semantics of a StackLang program.
prog :: Prog -> Domain
prog []    = \s -> Just s
prog (c:p) = \s -> case cmd c s of
                     Just s' -> prog p s'
                     Nothing -> Nothing


-- | Run a program on an initially empty stack.
--
--   >>> run ex2
--   Just [Right False]
--
--   >>> run (genSum [1..10])
--   Just [Left 55]
--
--   >>> run [PushN 3, Add, PushN 4]
--   Nothing
--
run :: Prog -> Maybe Stack
run p = prog p []