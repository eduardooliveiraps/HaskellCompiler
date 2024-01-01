import Data.List
import Data.Data 
import Data.Char 
import Distribution.Simple.Build (build)

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
If an element is a string, its value (either "tt" or "ff") is converted to "True" or "False" respectively.
-}
stack2Str :: Stack -> String 
stack2Str = intercalate "," . map (either show (\b -> if b == "tt" then "True" else "False"))

createEmptyState :: State -- Function that returns an empty machine’s state
createEmptyState = []

{-
This function converts a state, provided as input, into a string.
It concatenates state elements using commas with the "intercalate" function.
Each element in the state is transformed into a string of the form "variable=value", where the variable is the key and the value is either an integer converted to a string or the strings "tt" or "ff" converted to "True" or "False", respectively.
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
        (Right _, _) -> error "Run-time error: Invalid comparison with boolean value"
        (_, Right _) -> error "Run-time error: Invalid comparison with boolean value"
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
        Nothing -> error $ "Run-time error: Variable '" ++ var ++ "' not found in state"
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

-- Data type for arithmetic expressions
data Aexp = VarAexp String | IntAexp Integer | AddAexp Aexp Aexp | SubAexp Aexp Aexp | MultAexp Aexp Aexp
    deriving (Show)

-- Data type for boolean expressions
data Bexp = BoolBexp Bool | EqBexp Aexp Aexp | LeBexp Aexp Aexp | NotBexp Bexp | AndBexp Bexp Bexp
    deriving (Show)

-- Data type for statements
data Stm = AssignStm String Aexp | SeqStm Stm Stm | IfStm Bexp Stm Stm | WhileStm Bexp Stm | NoopStm
  deriving (Show)
type Program = [Stm] 

-- function that generates the code for an arithmetic expression
compA :: Aexp -> Code
compA (VarAexp var) = [Fetch var]  
compA (IntAexp n) = [Push n]       
compA (AddAexp e1 e2) = compA e2 ++ compA e1 ++ [Add]  
compA (SubAexp e1 e2) = compA e2 ++ compA e1 ++ [Sub]  
compA (MultAexp e1 e2) = compA e2 ++ compA e1 ++ [Mult]  

-- function that generates the code for a boolean expression
compB :: Bexp -> Code
compB (BoolBexp b) = if b then [Tru] else [Fals]  
compB (EqBexp e1 e2) = compA e2 ++ compA e1 ++ [Equ]  
compB (LeBexp e1 e2) = compA e2 ++ compA e1 ++ [Le]   
compB (NotBexp b) = compB b ++ [Neg]  
compB (AndBexp b1 b2) = compB b2 ++ compB b1 ++ [And]  

-- function that generates the code for a statement
compStm :: Stm -> Code
compStm (AssignStm var aexp) = compA aexp ++ [Store var]
compStm (SeqStm stm1 stm2) = compStm stm1 ++ compStm stm2
compStm (IfStm bexp stm1 stm2) = let code1 = compStm stm1
                                     code2 = compStm stm2
                                     codeB = compB bexp
                                 in codeB ++ [Branch code1 code2]
compStm (WhileStm bexp stm) = let codeS = compStm stm
                                  codeB = compB bexp
                              in [Loop codeB codeS]
compStm NoopStm = [Noop]

-- function that compiles the entire program (list of statements), generating the code for the program
compile :: Program -> Code
compile = concatMap compStm

-- function that takes a string and returns a list of tokens
lexer :: String -> [String]
lexer [] = []
lexer ('=':'=':cs) = "==" : lexer cs
lexer ('<':'=':cs) = "<=" : lexer cs
lexer (':':'=':cs) = ":=" : lexer cs
lexer (c:cs)
  | c `elem` "+-*/=<>()" = [c] : lexer cs  
  | isDigit c =
    let (num, rest) = span isDigit (c:cs)
    in num : lexer rest  
  | isLower c =
    let (var, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
    in var : lexer rest  
  | isAlpha c =
    let (keyword, rest) = span isAlpha (c:cs)
        keyword' = case keyword of
          "while" -> "while"
          "if" -> "if"
          "then" -> "then"
          "else" -> "else"
          _ -> keyword
    in keyword' : lexer rest  
  | c == ';' = ";" : lexer cs  
  | isSpace c = lexer cs  
  | otherwise = lexer cs  

-- Update parseAexp to handle parentheses
parseAexp :: [String] -> (Aexp, [String])
parseAexp tokens = parseAexp' tokens 0

parseAexp' :: [String] -> Int -> (Aexp, [String])
parseAexp' tokens level
  | level == 3 = parseFactor tokens
  | otherwise =
      let (left, tokens') = parseAexp' tokens (level + 1)
      in buildExpression left tokens' level

buildExpression :: Aexp -> [String] -> Int -> (Aexp, [String])
buildExpression left (op:tokens) level
  | op == operator level =
      let (right, tokens') = parseAexp' tokens (level + 1)
      in buildExpression (applyOperator level left right) tokens' level
  | otherwise = (left, op:tokens)
buildExpression left [] _ = (left, [])

operator :: Int -> String
operator level = ["+", "-", "*"] !! level

applyOperator :: Int -> Aexp -> Aexp -> Aexp
applyOperator level left right
  | level == 0 = AddAexp left right
  | level == 1 = SubAexp left right
  | otherwise = MultAexp left right

parseFactor :: [String] -> (Aexp, [String])
parseFactor ("(":rest) =
  let (aexp, rest1) = parseAexp rest
  in case rest1 of
    ")":rest2 -> (aexp, rest2)
    _ -> error "Syntax error: Missing closing parenthesis"
parseFactor (num:tokens)
  | all isDigit num = (IntAexp (read num), tokens)
  | otherwise = (VarAexp num, tokens)
parseFactor [] = error "Syntax error: Unexpected end of input"


-- function that takes a list of tokens and returns a tuple with the boolean expression and the remaining tokens
parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens =
  let (aexp1, rest1) = parseAexp tokens
      (op:rest2) = rest1
  in case op of
    "not" -> let (bexp, rest3) = parseBexp rest2 in (NotBexp bexp, rest3)
    "==" -> let (aexp2, rest3) = parseAexp rest2 in (EqBexp aexp1 aexp2, rest3)
    "<=" -> let (aexp2, rest3) = parseAexp rest2 in (LeBexp aexp1 aexp2, rest3)
    _ -> error $ "Syntax error: Invalid boolean expression " ++ op

parseStm :: [String] -> (Stm, [String])
parseStm [] = (NoopStm, [])
parseStm ("if":rest) = 
  let (bexp, "then":rest1) = parseBexp rest
      (stm1, rest2) = parseStm rest1
      (stm2, rest3) = case rest2 of
                        ";":"else":rest2' -> parseStm rest2'
                        "else":rest2' -> parseStm rest2'
                        _ -> error "Syntax error: 'if' statement not properly formed"
  in (IfStm bexp stm1 stm2, rest3)
parseStm ("while" : rest) =
  let (bexp, doToken : rest1) = parseBexp rest
      (stm, rest2) = parseStm rest1
  in if doToken /= "do"
     then error "Syntax error: 'while' statement not properly formed"
     else (WhileStm bexp stm, rest2)
parseStm (var:":=":rest) =
  let (aexp, rest1) = parseAexp rest
  in (AssignStm var aexp, rest1)
parseStm (";":rest) = parseStm rest
parseStm ("(":rest) = 
  let (block, rest1) = parseBlock rest
  in (block, rest1)
parseStm _ = error "Syntax error: Invalid statement"

parseBlock :: [String] -> (Stm, [String])
parseBlock [] = error "Syntax error: Missing closing parenthesis"
parseBlock (";":")":rest) = (NoopStm, rest)
parseBlock tokens = 
  let (stm, rest1) = parseStm tokens
  in if null rest1 || head rest1 == ")"
     then (stm, if null rest1 then [] else tail rest1)
     else let (stmtList, rest2) = parseBlock rest1
          in (SeqStm stm stmtList, rest2)

-- function that takes a list of tokens and returns a list of statements
buildData :: [String] -> Program
buildData [] = []
buildData tokens = 
  let (stm, rest) = parseStm tokens
  in stm : buildData rest

-- function that takes a string and returns a list of statements
parse :: String -> Program
parse = buildData . lexer

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
    where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
