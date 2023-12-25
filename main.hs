import Data.List

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Stack = [Either Integer Bool]

type State = [(String, Either Integer Bool)]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str = intercalate "," . map (either show (\b -> if b then "True" else "False"))

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str = intercalate "," . map (\(var, val) -> var ++ "=" ++ (either show (\b -> if b then "True" else "False") val)) . sort

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push i):code, stack, state) = run (code, (Left i):stack, state)
run ((Tru):code, stack, state) = run (code, (Right True):stack, state)
run ((Fals):code, stack, state) = run (code, (Right False):stack, state)
run ((Add):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 + i2)):stack, state)
run ((Sub):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 - i2)):stack, state)
run ((Mult):code, (Left i1):(Left i2):stack, state) = run (code, (Left (i1 * i2)):stack, state)
run ((Equ):code, (val1):(val2):stack, state) =
    case (val1, val2) of
        (Left i1, Left i2) -> run (code, (Right (i1 == i2)):stack, state)
        (Right b1, Right b2) -> run (code, (Right (b1 == b2)):stack, state)
        _ -> error "Run-time error"
run ((Le):code, (val1):(val2):stack, state) =
    case (val1, val2) of
        (Left i1, Left i2) -> run (code, (Right (i1 <= i2)):stack, state)
        _ -> error "Run-time error"
run ((And):code, (val1):(val2):stack, state) =
    case (val1, val2) of
        (Right b1, Right b2) -> run (code, (Right (b1 && b2)):stack, state)
        _ -> error "Run-time error"
run ((Neg):code, (val):stack, state) =
    case val of
        (Right b) -> run (code, (Right (not b)):stack, state)
        _ -> error "Run-time error"
run ((Store var):code, val:stack, state) = 
    let newState = filter ((/= var) . fst) state
    in run (code, stack, (var, val):newState)
run ((Fetch var):code, stack, state) = case lookup var state of
    Just val -> run (code, val:stack, state)
    Nothing -> error "Run-time error"
run ((Noop):code, stack, state) = run (code, stack, state)
run ((Branch code1 code2):code, (Right b):stack, state) =
    if b then run (code1 ++ code, stack, state)
    else run (code2 ++ code, stack, state)
run ((Branch _ _):code, stack, state) = error "Run-time error"
run ((Loop code1 code2):code, (Right b):stack, state) =
    if b then run (code1 ++ (Loop code1 code2):code, stack, state)
    else run (code2 ++ code, stack, state)
run ((Loop _ _):code, stack, state) = error "Run-time error"


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