# PFL 2nd Assignment - Assembler and Parser

## Group T09_G10, João António Teixeira Coelho (202004846) 50% contribution, Diogo Filipe Pereira Santos (202108747) 50% contribution

## Description

This project can be divided in two parts: the assembler and the parser.

The assembler consists of components responsible for interpreting and executing instructions within a low-level machine setup. In this context, it interacts with configurations of the form (c, e, s), where c represents a list of instructions or code to execute, e stands for the evaluation stack utilized for arithmetic and boolean expression evaluation, and s denotes the storage.

The parser encompasses compiler functions and parser functions. These functions are geared toward interpreting a small imperative programming language that incorporates arithmetic and boolean expressions, along with statement structures such as assignments (x := a), sequences of statements (instr1 ; instr2), if-then-else statements, and while loops.

## Part 1 - Assembler

Without altering the Inst and Code definitions, we started by defining the Stack and State types:

The `Stack` type is a list of StackMembers, which can either be Integers or Booleans.

The `State` type is a list of (String, StackMembers) tuples, which represent the current state of the storage in the assembler. Each tuple consists of a string representing the variable name and a StackMember representing the value stored in that variable.

The next step was to define the functions that would be used to create a Stack and a State, to use in the Assembler.
`createEmptyStack` creates an empty stack, and `createEmptyState` does the same for the State.

After that, we defined the functions that translated the Stack and State to a String, `stack2Str` and `state2Str` respectively.

`stack2Str` takes a Stack and converts it to a string with commas separating the different values. This is done by using the `valueToStr` helper function recursively for all elements in the stack using the go helper function. For the last element, we don't append a comma. We manage this by checking if the result string is not empty before removing the last character, which is expected to be a comma.

`state2Str` converts a State. It sorts the state alphabetically using `sortState` and formats it into a string via `formatState`. `formatState` can handle empty, single-element, and multi-element states, leaving no comma at the end, by checking if the result string is not empty before removing the last character. `valueToStr` converts StackMembers into strings using `show`.

To finish Part 1, we created the `run` function.
It interprets a list of instructions (Code), a stack, and a state (storage), using recursion to process each instruction in the list until it is empty.

The processing of an instruction depends on the type of the instruction, which is determined using another level of pattern matching. 

For example, if the instruction is `Push n`, we place the value n at the head of the stack list and proceed with the recursion.

`Add` and `Mult` and `Sub` assign the top two values of the stack to x and y, and then place the result of the corresponding operation at the head of the stack list. They all proceed with the recursion and all of them place the top element before the operation sign (Important in `Sub`). They only work with Integers, returning an error if the top two elements of the stack are not Ints.

`Equ` and `Le` compare the top two values of the stack and place the result of the corresponding operation at the head of the stack list. Both proceed with the recursion. The top element is placed before the operation sign (Important in `Le`). `Equ` works if the top two elements of the stack are both Integers or both Booleans, returning an error if they are not while `Le` only works with Integers, returning an error if the top two elements of the stack are not Ints.

`Neg` negates the top element of the stack and places it at the top, proceeding with the recursion. It works with Booleans and Ints.

`And` only works with Booleans, returning an error if the top two elements are not that. It performs the logical AND operation on the top two elements of the stack and places the result at the head of the stack list, proceeding with the recursion.

`Tru` and `Fals` place the corresponding Boolean value at the head of the stack list and proceed with the recursion.

`Fetch` searches for variable x in the state and places its value at the top of the stack. Proceeds with the recursion. Throws an error if the variable is not found.

`Store x` assigns the top element of the stack to variable x in the state, removing the variable if it exists and updating its value. Proceeds with the recursion. Throws an error if the top element of the stack isn't an Integer or a Boolean.

`Noop` does nothing and proceeds with the recursion.

`Branch C1 C2` checks the top element of the stack. If it is True, it executes C1, otherwise it executes C2. Proceeds with the recursion. Throws an error if the top element of the stack isn't a Boolean.

`Loop C1 C2` executes C1, then checks the top element of the stack. If it is True, it executes C2, then executes the Loop again. Proceeds with the recursion. Throws an error if the top element of the stack isn't a Boolean.


## Part 2 - Parser

For the parser, we start by defining 3 data types to represent the statements and expressions of the language:
`Aexp`, `Bexp` and `Stm`.

`Aexp` represents arithmetic expressions, and can be either a constant, a variable, or an operation between two `Aexp`s.

`Bexp` represents boolean expressions, and can be either True or False, an operation involving comparisons of integers or booleans or the not operation, that negates a boolean expression.

Lastly, `Stm` represents statements, and can be either an assignment, an if-then-else statement or a while loop.

We define the `compile` function, that takes a list of statements and returns a list of instructions (Code) that can be interpreted by the Assembler.

We have two auxiliary functions, `compA` and `compB`, that handle the logic of compiling arithmetic and boolean expressions respectively.

They are then called in the `compile` function, which uses pattern matching to determine the type of the statement and then calls the corresponding function.

For `Assignment` statements, it processes an assignment by compiling the expression into instructions and storing the result in the variable using [Store var]. Then, it continues to compile the rest of the statements by calling compile on the remaining statements.

For `Conditional` statements, `compile` generates instructions for the conditional expression using compB. It creates a branching instruction [Branch (compile thenStm) (compile elseStm)] based on the compiled thenStm and elseStm, continuing to compile the remaining statements.

Lastly, for `WhileLoop` statements, `compile` generates a looping instruction using Loop (compB bexp) (compile doStm). It compiles the loop condition with compB and the loop body by recursively calling compile. It then proceeds to compile the remaining statements.

We now start describing how we implemented the parser function.

### Parser

We define the `Token` type. It has every possible symbol that can be used in the language, as well as a token for Variables and one for Integers (constants).

The `lexer` function takes a string and returns a list of tokens. It processes a string character by character, converting each character or sequence of characters into the respective tokens. When it finds a space, it skips it and continues computing. When a digit appears, the `isDigit` function is used to check if the next character is also a digit. When this is false, it maps the number to `IntToken`, with the help of the `read` function. When a letter appears, the `isAlpha` function is used to check if the next character is also a letter. When this is false, it maps the word to `VarToken`. Invalid characters are handled by returning an error.

The `buildData` function takes a list of tokens and returns a list of statements, to be used in the compile function. When it encounters a `;`, the function ignores it and continues processing the remaining tokens. `VarToken` followed by `:=` calls for the `buildVar` function, that builds an assignment statement by parsing the arithmetic expression and recursively building the rest of the program. `IfToken` initiates the `buildIf` process, which constructs a conditional statement by parsing the boolean expression and subsequent then and else branches. `WhileToken` triggers the `buildWhile` function, that constructs a while loop by parsing the boolean expression and the associated loop body.

We then have specific functions to parse each type of expression and statement.
We will highlight `parseAexp` and `parseBexp`.

In `parseAexp`, we call for `parseAddSub`, our entry point into the parsing of arithmetic expressions. If it finds a token different than `+`, it calls for the `parseMult`function, that does the same with the `parseValue` function when it doesn't encounter a `*`. In the case that something that's not an aexp is passed to parseAexp, it returns an error.

As for `parseBexp`, the entry point is `parseAnd`. It handles finding different tokens in a similar way to the function mentioned above. If an expression that's not a bexp is passed to parseBexp, it returns an error.

The last functions to mention are `getTokensBetweenPs` and `getTokensBetweenPs'`. They are used to obtain the tokens between parentheses. `getTokensBetweenPs` is the entry point, and it calls `getTokensBetweenPs'` with three accumulators: one for the remaining tokens, one for the stack of parentheses, and one for the collected tokens within the parentheses. `getTokensBetweenPs'` is a recursive function that adds the tokens to the accumulator until it finds a closing parenthesis, returning the accumulator when it does. It does so by adding opening parenthesis to the stack, when one is found. When a closing parenthesis is found, it pops the stack, and once it's empty, it reverses and returns the collected tokens.

Lastly, the `parse` function function is a composition of buildData and lexer. It takes a string as input and processes it through two stages: lexical analysis (lexer) and syntactic analysis (buildData).

Firstly, it applies the lexer function to the input string, breaking it down into a sequence of tokens representing various elements and structures of the program.

Once the string is tokenized, the buildData function processes these tokens, interpreting their arrangement and structure to generate a sequence of statements, that will be handled by the compiler.

## How to run
To run the code, simply load main.hs in ghci and run the function `main`. This will run all of the given tests (that don't produce errors) and print the results.

## Conclusion
This project allowed us to expand our Haskell knowledge significantly. We are now more familiar with its syntax and with the way it handles data types and pattern matching, which will prove useful in the future, as this project has jumpstarted our knowledge for the mini-test.

## References
https://wiki.haskell.org/Parsing_a_simple_imperative_language
https://wiki.haskell.org/Parsing_expressions_and_statements
Moodle slides
