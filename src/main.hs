-- Part 1

import Data.Char (isAlphaNum, isDigit, isLower, isSpace)
import Data.List (sortOn)

-- Do not modify our definition of Inst and Code
data Inst
  = Push Integer
  | Add
  | Mult
  | Sub
  | Tru
  | Fals
  | Equ
  | Le
  | And
  | Neg
  | Fetch String
  | Store String
  | Noop
  | Branch Code Code
  | Loop Code Code
  deriving (Show)

type Code = [Inst]

-- creates the type that will be used to represent the stack, can be either an integer or a boolean
data StackMembers = Integers Integer | Booleans Bool deriving (Show, Eq)

-- defines the Stack, a list of StackMembers
type Stack = [StackMembers]

-- creates an empty stack that will be used to start the program
createEmptyStack :: Stack
createEmptyStack = []

-- creates the type that will be used to represent the state, a list of tuples
type State = [(String, StackMembers)]

-- creates an empty state that will be used to start the program
createEmptyState :: State
createEmptyState = []

-- converts the stack to a string
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (x : xs) = go xs (valueToStr x) -- if the stack is not empty, call the go function
  where
    valueToStr :: StackMembers -> String -- converts the stack member to a string
    valueToStr (Integers n) = show n ++ ","
    valueToStr (Booleans b) = show b ++ ","

    go :: Stack -> String -> String -- goes through the stack and adds the stack members to the string
    go [] result = if null result then result else init result -- if the result is empty, return the result, otherwise remove the last comma
    go (y : ys) result = go ys (result ++ valueToStr y) -- if the stack is not empty, call the go function again

-- converts the state to a string, calling the formatState and sortState functions
state2Str :: State -> String
state2Str state = formatState $ sortState state

-- formats the state to a string
formatState :: State -> String
formatState [] = ""
formatState [(var, value)] = var ++ "=" ++ valueToStr value -- if the state has only one element, return the element
  where
    valueToStr :: StackMembers -> String -- converts the stack member to a string
    valueToStr (Integers n) = show n
    valueToStr (Booleans b) = show b
formatState ((var, value) : xs) = var ++ "=" ++ valueToStr value ++ "," ++ formatState xs -- if the state has more than one element, call the formatState function again
  where
    valueToStr :: StackMembers -> String
    valueToStr (Integers n) = show n
    valueToStr (Booleans b) = show b

-- sorts the state by the variable name
sortState :: State -> State
sortState = sortOn fst

-- interpreter that takes the code and an empty stack and state and returns the stack and state after the program has been executed and empty code
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, storage) = ([], stack, storage)
run (instruction : code, stack, storage) = case instruction of
  Push n -> run (code, Integers n : stack, storage) -- puts n at top of stack and calls the run function again
  Add -> case stack of
    (Integers x : Integers y : rest) -> run (code, Integers (x + y) : rest, storage) -- adds the top two elements, puts the result at the top of the stack and calls the run function again 
    _ -> error "Run-time error"
  Mult -> case stack of
    (Integers x : Integers y : rest) -> run (code, Integers (x * y) : rest, storage) -- multiplies the top two elements, puts the result at the top of the stack and calls the run function again
    _ -> error "Run-time error"
  Sub -> case stack of
    (Integers x : Integers y : rest) -> run (code, Integers (x - y) : rest, storage) -- subtracts the top two elements (top - second), puts the result at the top of the stack and calls the run function again
    _ -> error "Run-time error"
  Equ -> case stack of
    (x : y : rest) -> run (code, Booleans (x == y) : rest, storage) -- checks if the top two elements are equal, puts the result at the top of the stack and calls the run function again
    _ -> error "Run-time error"
  Le -> case stack of
    (Integers x : Integers y : rest) -> run (code, Booleans (x <= y) : rest, storage) -- checks if the top element is less than or equal to the second, puts the result at the top of the stack and calls the run function again
    _ -> error "Run-time error"
  Neg -> case stack of
    (Integers x : rest) -> run (code, Integers (-x) : rest, storage) -- negates the top element, puts the result at the top of the stack and calls the run function again
    (Booleans b : rest) -> run (code, Booleans (not b) : rest, storage) -- negates the top element, puts the result at the top of the stack and calls the run function again
    _ -> error "Run-time error"
  And -> case stack of
    (Booleans x : Booleans y : rest) -> run (code, Booleans (x && y) : rest, storage) -- checks if the top two elements are true, puts the result at the top of the stack and calls the run function again
    _ -> error "Run-time error"
  Tru -> run (code, Booleans True : stack, storage) -- puts true at top of stack and calls the run function again
  Fals -> run (code, Booleans False : stack, storage) -- puts false at top of stack and calls the run function again
  Fetch var -> case lookup var storage of
    Just val -> run (code, val : stack, storage) -- looks up the variable in the state and puts the value at the top of the stack, calls the run function again
    Nothing -> error "Run-time error"
  Store var -> case stack of
    (val : rest) -> run (code, rest, (var, val) : filter (\(v, _) -> v /= var) storage) -- removes the variable from the stack if it exists, adds the variable and value to the state and calls the run function again
    _ -> error "Run-time error"
  Noop -> run (code, stack, storage) -- does nothing and calls the run function again
  Branch c1 c2 -> case stack of
    (Booleans True : rest) -> run (c1 ++ code, rest, storage) -- if the top element is true, executes c1 and calls the run function again
    (Booleans False : rest) -> run (c2 ++ code, rest, storage) -- if the top element is false, executes c2 and calls the run function again
    _ -> error "Run-time error"
  Loop c1 c2 -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, storage) -- executes c1, evaluates top element, if true executes c2 and calls the run function again

-- Part 2

-- Arithmetic Expressions
data Aexp
  = Value Integer
  | Variable String
  | Addition Aexp Aexp
  | Subtraction Aexp Aexp
  | Multiplication Aexp Aexp
  deriving (Show)

-- Boolean Expressions
data Bexp
  = MyTrue
  | MyFalse
  | NotOperation Bexp
  | Equals Aexp Aexp
  | EqualsBool Bexp Bexp
  | LessThanOrEqual Aexp Aexp
  | AndOperation Bexp Bexp
  deriving (Show)

-- Statements
data Stm
  = Assignment String Aexp
  | Conditional Bexp [Stm] [Stm]
  | WhileLoop Bexp [Stm]
  deriving (Show)

-- Compiles the statements into code
compile :: [Stm] -> Code
compile [] = []
compile (Assignment var aexp : stms) = compA aexp ++ [Store var] ++ compile stms -- compiles the arithmetic expression, stores the result in the variable and calls the compile function again
compile (Conditional bexp thenStm elseStm : stms) = compB bexp ++ [Branch (compile thenStm) (compile elseStm)] ++ compile stms -- compiles the boolean expression, executes the then statement if true, otherwise executes the else statement and calls the compile function again
compile (WhileLoop bexp doStm : stms) = Loop (compB bexp) (compile doStm) : compile stms -- compiles the boolean expression, executes the do statement if true and calls the compile function again

-- Compiles arithmetic expressions into code
compA :: Aexp -> Code
compA (Value n) = [Push n]
compA (Variable var) = [Fetch var]
compA (Addition a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (Subtraction a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (Multiplication a1 a2) = compA a2 ++ compA a1 ++ [Mult]

-- Compiles boolean expressions into code
compB :: Bexp -> Code
compB MyTrue = [Tru]
compB MyFalse = [Fals]
compB (NotOperation b) = compB b ++ [Neg]
compB (Equals a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (EqualsBool b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (LessThanOrEqual a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (AndOperation b1 b2) = compB b2 ++ compB b1 ++ [And]

-- data type that stores every character that can be used in the language
data Token
  = IntToken Integer
  | VarToken String
  | PlusToken
  | MinusToken
  | MultToken
  | EqToken
  | LeToken
  | AndToken
  | NotToken
  | TrueToken
  | FalseToken
  | IfToken
  | ThenToken
  | ElseToken
  | WhileToken
  | BoolEqToken
  | SemiColonToken
  | OpenParenthesisToken
  | CloseParenthesisToken
  | AssignmentToken
  | DoToken
  deriving (Show, Eq)

-- function that maps characters that appear in the string to tokens
lexer :: String -> [Token]
lexer [] = []
lexer ('+' : rest) = PlusToken : lexer rest
lexer ('-' : rest) = MinusToken : lexer rest
lexer ('*' : rest) = MultToken : lexer rest
lexer ('=' : '=' : rest) = EqToken : lexer rest
lexer ('<' : '=' : rest) = LeToken : lexer rest
lexer ('a' : 'n' : 'd' : rest) = AndToken : lexer rest
lexer ('n' : 'o' : 't' : rest) = NotToken : lexer rest
lexer ('T' : 'r' : 'u' : 'e' : rest) = TrueToken : lexer rest
lexer ('F' : 'a' : 'l' : 's' : 'e' : rest) = FalseToken : lexer rest
lexer ('i' : 'f' : rest) = IfToken : lexer rest
lexer ('t' : 'h' : 'e' : 'n' : rest) = ThenToken : lexer rest
lexer ('e' : 'l' : 's' : 'e' : rest) = ElseToken : lexer rest
lexer ('w' : 'h' : 'i' : 'l' : 'e' : rest) = WhileToken : lexer rest
lexer ('=' : rest) = BoolEqToken : lexer rest
lexer (';' : rest) = SemiColonToken : lexer rest
lexer ('(' : rest) = OpenParenthesisToken : lexer rest
lexer (')' : rest) = CloseParenthesisToken : lexer rest
lexer (':' : '=' : rest) = AssignmentToken : lexer rest
lexer ('d' : 'o' : rest) = DoToken : lexer rest
lexer (x : rest)
  | isSpace x = lexer rest
  | isDigit x = IntToken (read (x : takeWhile isDigit rest)) : lexer (dropWhile isDigit rest)
  | isLower x = VarToken (x : takeWhile isAlphaNum rest) : lexer (dropWhile isAlphaNum rest)
  | otherwise = error ("Invalid character: " ++ [x])

-- function that takes a list of tokens and returns a list of statements, to use in the compilers
buildData :: [Token] -> [Stm]
buildData [] = []
buildData (SemiColonToken : remainingTokens) = buildData remainingTokens -- if the token is a semicolon, ignore it and call the buildData function again
buildData (VarToken var : AssignmentToken : remainingTokens) = buildVar (VarToken var : AssignmentToken : remainingTokens) -- if the token is a variable and is followed by an assignment operator, call the buildVar function
buildData (IfToken : remainingTokens) = buildIf (IfToken : remainingTokens)  -- if the token is an if, call the buildIf function
buildData (WhileToken : remainingTokens) = buildWhile (WhileToken : remainingTokens) -- if the token is a while, call the buildWhile function
buildData _ = error "Invalid program"

-- allows the assignment of variables, by extracting the variable and parsing aexp, constructing an assignment
buildVar :: [Token] -> [Stm]
buildVar (VarToken var : AssignmentToken : remainingTokens) = Assignment var (parseAexp aexp) : buildData remainder
  where
    (aexp, remainder) = break (== SemiColonToken) remainingTokens
buildVar _ = error "Invalid program for variable assignment"

-- allows the construction of if statements, by parsing the boolean expression and the then and else branches, constructing a conditional
buildIf :: [Token] -> [Stm]
buildIf (IfToken : remainingTokens) = Conditional (parseBexp bexp) (buildData thenTokens) (buildData elseTokens) : buildData remainder
  where
    (bexp, thenBranchTokens) = break (== ThenToken) remainingTokens
    tokensAfterThen = tail thenBranchTokens
    (thenTokens, withElseTokens) =
      if head tokensAfterThen == OpenParenthesisToken
        then getTokensBetweenPs tokensAfterThen
        else break (== SemiColonToken) tokensAfterThen
    remainingTokensAfterElse =
      if head withElseTokens == SemiColonToken
        then drop 2 withElseTokens
        else tail withElseTokens
    (elseTokens, remainder) =
      if head remainingTokensAfterElse == OpenParenthesisToken
        then getTokensBetweenPs remainingTokensAfterElse
        else break (== SemiColonToken) remainingTokensAfterElse
buildIf _ = error "Invalid program for conditional statement"

-- allows the construction of while loops, by parsing the boolean expression and the do branch, constructing a while loop
buildWhile :: [Token] -> [Stm]
buildWhile (WhileToken : remainingTokens) = WhileLoop (parseBexp bexp) (buildData doTokens) : buildData remainder
  where
    (bexp, withDoTokens) = break (== DoToken) remainingTokens
    (doTokens, remainder) =
      if head (tail withDoTokens) == OpenParenthesisToken
        then getTokensBetweenPs (tail withDoTokens)
        else break (== SemiColonToken) (tail withDoTokens)
buildWhile _ = error "Invalid program for while loop"

-- parses arithmetic expressions
parseAexp :: [Token] -> Aexp
parseAexp tokens =
  case parseAddSub tokens of
    Just (aexp, []) -> aexp
    Just _ -> error "Reached parseAexp with extra tokens"
    Nothing -> error "Reached parseAexp with invalid tokens"

-- parses addition and subtraction, calls for parseMult if the token is not a plus or minus
parseAddSub :: [Token] -> Maybe (Aexp, [Token])
parseAddSub tokens = case parseMult tokens of
  Just (aexp1, PlusToken : rest) -> case parseAddSub rest of
    Just (aexp2, rest') -> Just (Addition aexp1 aexp2, rest')
    Nothing -> Nothing
  Just (aexp1, MinusToken : rest) -> case parseAddSub rest of
    Just (aexp2, rest') -> Just (Subtraction aexp1 aexp2, rest')
    Nothing -> Nothing
  result -> result

-- parses multiplication, calls for parseValue if the token is not a multiplication
parseMult :: [Token] -> Maybe (Aexp, [Token])
parseMult tokens = case parseValue tokens of
  Just (aexp1, MultToken : rest) -> case parseMult rest of
    Just (aexp2, rest') -> Just (Multiplication aexp1 aexp2, rest')
    Nothing -> Nothing
  result -> result

-- parses the processing of variables and constants, calls for parseAddSub if the token is not a variable or constant
parseValue :: [Token] -> Maybe (Aexp, [Token])
parseValue (IntToken n : rest) = Just (Value n, rest)
parseValue (VarToken var : rest) = Just (Variable var, rest)
parseValue (OpenParenthesisToken : rest') = case parseAddSub rest' of
  Just (aexp, CloseParenthesisToken : rest'') -> Just (aexp, rest'')
  Just _ -> Nothing
  Nothing -> Nothing
parseValue _ = Nothing

-- parses boolean expressions
parseBexp :: [Token] -> Bexp
parseBexp tokens =
  case parseAnd tokens of
    Just (bexp, []) -> bexp
    Just _ -> error "Reached parseBexp with extra tokens"
    Nothing -> error "Reached parseBexp with invalid tokens"

-- parses and, calls for parseBoolEquality if the token is not an and
parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens = case parseBoolEquality tokens of
  Just (bexp1, AndToken : rest) -> case parseAnd rest of
    Just (bexp2, rest') -> Just (AndOperation bexp1 bexp2, rest')
    Nothing -> error "Reached parseAnd with invalid tokens"
  Just (bexp, rest) -> Just (bexp, rest)
  Nothing -> Nothing

-- parses equality, calls for parseNot if the token is not =
parseBoolEquality :: [Token] -> Maybe (Bexp, [Token])
parseBoolEquality tokens = case parseNot tokens of
  Just (bexp1, BoolEqToken : rest) -> case parseBoolEquality rest of
    Just (bexp2, rest') -> Just (EqualsBool bexp1 bexp2, rest')
    Nothing -> error "Reached parseBoolEquality with invalid tokens"
  Just (bexp, rest) -> Just (bexp, rest)
  Nothing -> Nothing

-- parses not, calls for parseIntComparison if the token is not not
parseNot :: [Token] -> Maybe (Bexp, [Token])
parseNot (NotToken : rest) = case parseNot rest of
  Just (bexp, rest') -> Just (NotOperation bexp, rest')
  Nothing -> error "Reached parseNot with invalid tokens"
parseNot tokens = parseIntComparison tokens

-- parses integer comparison, calls for parseTrueFalse if the token is not == or <=
parseIntComparison :: [Token] -> Maybe (Bexp, [Token])
parseIntComparison tokens = case parseAddSub tokens of
  Just (aexp1, EqToken : rest) -> case parseAddSub rest of
    Just (aexp2, rest') -> Just (Equals aexp1 aexp2, rest')
    Nothing -> parseTrueFalse tokens
  Just (aexp1, LeToken : rest) -> case parseAddSub rest of
    Just (aexp2, rest') -> Just (LessThanOrEqual aexp1 aexp2, rest')
    Nothing -> parseTrueFalse tokens
  _ -> parseTrueFalse tokens

-- parses true and false, calls for parseAnd if the token is not true or false
parseTrueFalse :: [Token] -> Maybe (Bexp, [Token])
parseTrueFalse (TrueToken : rest) = Just (MyTrue, rest)
parseTrueFalse (FalseToken : rest) = Just (MyFalse, rest)
parseTrueFalse (OpenParenthesisToken : rest) = case parseAnd rest of
  Just (bexp, CloseParenthesisToken : rest') -> Just (bexp, rest')
  _ -> Nothing
parseTrueFalse _ = Nothing

-- wrapper function for getTokensBetweenPs'
getTokensBetweenPs :: [Token] -> ([Token], [Token])
getTokensBetweenPs tokens = (contentTokens, remainingTokens)
  where
    (remainingTokens, _, i_contentTokens) = getTokensBetweenPs' tokens "" []
    contentTokens = tail (init i_contentTokens)

-- function that gets the tokens between parentheses
getTokensBetweenPs' :: [Token] -> String -> [Token] -> ([Token], String, [Token])
getTokensBetweenPs' [] stack result = ([], "", reverse result)
getTokensBetweenPs' (OpenParenthesisToken : tokens) stack result =
  getTokensBetweenPs' tokens ('(' : stack) (OpenParenthesisToken:result)
getTokensBetweenPs' (CloseParenthesisToken : tokens) (top : stack) result =
  getTokensBetweenPs' tokens stack (CloseParenthesisToken:result)
getTokensBetweenPs' (token : tokens) stack rest
  | null stack = (token : tokens, "", reverse rest)
  | otherwise = getTokensBetweenPs' tokens stack (token : rest)

-- function that parses the program, composotion of lexer and buildData
parse :: String -> [Stm]
parse = buildData . lexer

-- Tests

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Test Assembler"
  putStrLn ""

  putStrLn "Test 1: "
  let expected1 = ("-10", "")
  let result1 = testAssembler [Push 10, Push 4, Push 3, Sub, Mult]
  putStrLn $ "Expected Outcome: " ++ show expected1
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "Comparison: " ++ show (result1 == expected1)
  putStrLn ""

  putStrLn "Test 2: "
  let expected2 = ("", "a=3,someVar=False,var=True")
  let result2 = testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]
  putStrLn $ "Expected Outcome: " ++ show expected2
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Comparison: " ++ show (result2 == expected2)
  putStrLn ""

  putStrLn "Test 3: "
  let expected3 = ("False", "var=False")
  let result3 = testAssembler [Fals, Store "var", Fetch "var"]
  putStrLn $ "Expected Outcome: " ++ show expected3
  putStrLn $ "Result: " ++ show result3
  putStrLn $ "Comparison: " ++ show (result3 == expected3)
  putStrLn ""

  putStrLn "Test 4: "
  let expected4 = ("False,True,-20", "")
  let result4 = testAssembler [Push (-20), Tru, Fals]
  putStrLn $ "Expected Outcome: " ++ show expected4
  putStrLn $ "Result: " ++ show result4
  putStrLn $ "Comparison: " ++ show (result4 == expected4)
  putStrLn ""

  putStrLn "Test 5: "
  let expected5 = ("False,True,-20", "")
  let result5 = testAssembler [Push (-20), Tru, Tru, Neg]
  putStrLn $ "Expected Outcome: " ++ show expected5
  putStrLn $ "Result: " ++ show result5
  putStrLn $ "Comparison: " ++ show (result5 == expected5)
  putStrLn ""

  putStrLn "Test 6: "
  let expected6 = ("False,-20", "")
  let result6 = testAssembler [Push (-20), Tru, Tru, Neg, Equ]
  putStrLn $ "Expected Outcome: " ++ show expected6
  putStrLn $ "Result: " ++ show result6
  putStrLn $ "Comparison: " ++ show (result6 == expected6)
  putStrLn ""

  putStrLn "Test 7: "
  let expected7 = ("True", "")
  let result7 = testAssembler [Push (-20), Push (-21), Le]
  putStrLn $ "Expected Outcome: " ++ show expected7
  putStrLn $ "Result: " ++ show result7
  putStrLn $ "Comparison: " ++ show (result7 == expected7)
  putStrLn ""

  putStrLn "Test 8: "
  let expected8 = ("", "x=4")
  let result8 = testAssembler [Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"]
  putStrLn $ "Expected Outcome: " ++ show expected8
  putStrLn $ "Result: " ++ show result8
  putStrLn $ "Comparison: " ++ show (result8 == expected8)
  putStrLn ""

  putStrLn "Test 9: "
  let expected9 = ("", "fact=3628800,i=1")
  let result9 = testAssembler [Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]]
  putStrLn $ "Expected Outcome: " ++ show expected9
  putStrLn $ "Result: " ++ show result9
  putStrLn $ "Comparison: " ++ show (result9 == expected9)
  putStrLn ""

  putStrLn ""
  putStrLn "Test Parser"
  putStrLn ""
  putStrLn ""

  putStrLn "Test 1: "
  let expected1 = ("", "x=4")
  let result1 = testParser "x := 5; x := x - 1;"
  putStrLn $ "Expected Outcome: " ++ show expected1
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "Comparison: " ++ show (result1 == expected1)
  putStrLn ""

  putStrLn "Test 2: "
  let expected2 = ("", "x=-2")
  let result2 = testParser "x := 0 - 2;"
  putStrLn $ "Expected Outcome: " ++ show expected2
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Comparison: " ++ show (result2 == expected2)
  putStrLn ""

  putStrLn "Test 3: "
  let expected3 = ("", "y=2")
  let result3 = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
  putStrLn $ "Expected Outcome: " ++ show expected3
  putStrLn $ "Result: " ++ show result3
  putStrLn $ "Comparison: " ++ show (result3 == expected3)
  putStrLn ""

  putStrLn "Test 4: "
  let expected4 = ("", "x=1")
  let result4 = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"
  putStrLn $ "Expected Outcome: " ++ show expected4
  putStrLn $ "Result: " ++ show result4
  putStrLn $ "Comparison: " ++ show (result4 == expected4)
  putStrLn ""

  putStrLn "Test 5: "
  let expected5 = ("", "x=2")
  let result5 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
  putStrLn $ "Expected Outcome: " ++ show expected5
  putStrLn $ "Result: " ++ show result5
  putStrLn $ "Comparison: " ++ show (result5 == expected5)
  putStrLn ""

  putStrLn "Test 6: "
  let expected6 = ("", "x=2,z=4")
  let result6 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
  putStrLn $ "Expected Outcome: " ++ show expected6
  putStrLn $ "Result: " ++ show result6
  putStrLn $ "Comparison: " ++ show (result6 == expected6)
  putStrLn ""

  putStrLn "Test 7: "
  let expected7 = ("", "x=34,y=68")
  let result7 = testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"
  putStrLn $ "Expected Outcome: " ++ show expected7
  putStrLn $ "Result: " ++ show result7
  putStrLn $ "Comparison: " ++ show (result7 == expected7)
  putStrLn ""

  putStrLn "Test 8: "
  let expected8 = ("", "x=34")
  let result8 = testParser "x := 42; if x <= 43 then (x := 33; x := x+1;); else x := 1;"
  putStrLn $ "Expected Outcome: " ++ show expected8
  putStrLn $ "Result: " ++ show result8
  putStrLn $ "Comparison: " ++ show (result8 == expected8)
  putStrLn ""

  putStrLn "Test 9: "
  let expected9 = ("", "x=1")
  let result9 = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"
  putStrLn $ "Expected Outcome: " ++ show expected9
  putStrLn $ "Result: " ++ show result9
  putStrLn $ "Comparison: " ++ show (result9 == expected9)
  putStrLn ""

  putStrLn "Test 10: "
  let expected10 = ("", "x=2")
  let result10 = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"
  putStrLn $ "Expected Outcome: " ++ show expected10
  putStrLn $ "Result: " ++ show result10
  putStrLn $ "Comparison: " ++ show (result10 == expected10)
  putStrLn ""

  putStrLn "Test 11: "
  let expected11 = ("", "x=2,y=-10,z=6")
  let result11 = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
  putStrLn $ "Expected Outcome: " ++ show expected11
  putStrLn $ "Result: " ++ show result11
  putStrLn $ "Comparison: " ++ show (result11 == expected11)
  putStrLn ""

  putStrLn "Test 12: "
  let expected12 = ("", "fact=3628800,i=1")
  let result12 = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
  putStrLn $ "Expected Outcome: " ++ show expected12
  putStrLn $ "Result: " ++ show result12
  putStrLn $ "Comparison: " ++ show (result12 == expected12)
  putStrLn ""

