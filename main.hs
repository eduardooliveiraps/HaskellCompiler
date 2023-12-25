import Data.List

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code

-- Definition of the machine's instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst] -- A type synonym representing a sequence of instructions used in the machine.

type Stack = [Either Integer String] -- A type that represents the machine’s stack

type State = [(String, Either Integer String)] -- A type that represents the machine’s state

createEmptyStack :: Stack -- Function that returns an empty machine’s stack
createEmptyStack = []

{-
This function converts a stack, provided as input, into a string.
It concatenates stack elements using commas with the "intercalate" function.
If an element is an integer, it is converted to a string using the "show" function.
If the element is boolean, its value will be transformed into the string "True" or "False".
-}
stack2Str :: Stack -> String 
stack2Str = intercalate "," . map (either show (\b -> if b == "tt" then "True" else "False"))

createEmptyState :: State -- Function that returns an empty machine’s state
createEmptyState = []

{-
This function converts a state, provided as input, into a string.
It concatenates state elements using commas with the "intercalate" function.
Each element in the state is transformed into a string of the form "variable=value", where the variable is the key and the value is either an integer converted to a string or a boolean converted to "True" or "False".
The entries are sorted based on variable names for consistent output.
-}
state2Str :: State -> String 
state2Str = intercalate "," . map (\(var, val) -> var ++ "=" ++ (either show (\b -> if b == "tt" then "True" else "False") val)) . sort

{-
This function takes as input a tuple of the form (Code, Stack, State) and returns a tuple of the same form.
The first element of the input tuple is a list of instructions, the second element is a stack and the third element is a state.
The function executes the instructions in the list, updating the stack and the state accordingly.
The function is recursive and the execution of the instructions is done by pattern matching on the first instruction in the list.
-}
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push i):code, stack, state) = run (code, (Left i):stack, state)
run ((Tru):code, stack, state) = run (code, (Right "tt"):stack, state)
run ((Fals):code, stack, state) = run (code, (Right "ff"):stack, state)
run ((Add):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 + i2)):stack, state)
run ((Sub):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 - i2)):stack, state)
run ((Mult):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 * i2)):stack, state)
run ((Equ):code, (val1):(val2):stack, state) =
    case (val1, val2) of
        (Left i1, Left i2) -> run (code, (Right (if i1 == i2 then "tt" else "ff")):stack, state)
        (Right b1, Right b2) -> run (code, (Right (if b1 == b2 then "tt" else "ff")):stack, state)
        _ -> error "Run-time error"
run ((Le):code, (val1):(val2):stack, state) =
    case (val1, val2) of
        (Left i1, Left i2) -> run (code, (Right (if i1 <= i2 then "tt" else "ff")):stack, state)
        _ -> error "Run-time error"
run ((And):code, (val1):(val2):stack, state) =
    case (val1, val2) of
        (Right b1, Right b2) -> run (code, (Right (if b1 == "tt" && b2 == "tt" then "tt" else "ff")):stack, state)
        _ -> error "Run-time error"
run ((Neg):code, (val):stack, state) =
    case val of
        (Right b) -> run (code, (Right (if b == "tt" then "ff" else "tt")):stack, state)
        _ -> error "Run-time error"
run ((Store var):code, val:stack, state) = 
    let newState = (var, val) : filter ((/= var) . fst) state
    in run (code, stack, newState)
run ((Fetch var):code, stack, state) = 
    case lookup var state of
    Just val -> run (code, val:stack, state)
    Nothing -> error "Run-time error"
run ((Noop):code, stack, state) = run (code, stack, state)
run ((Branch code1 code2):code, (Right b):stack, state) =
    if b == "tt" then run (code1 ++ code, stack, state)
    else run (code2 ++ code, stack, state)
run ((Branch _ _):code, stack, state) = error "Run-time error"
run ((Loop code1 code2):code, stack, state) =
    let branchInstruction = Branch (code2 ++ [Loop code1 code2]) [Noop]
    in run (code1 ++ [branchInstruction], stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")