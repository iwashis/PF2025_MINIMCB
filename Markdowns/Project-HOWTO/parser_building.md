# Building a Parser for the Dining Philosophers Language

This tutorial will guide you through creating a parser for a domain-specific language (DSL) designed to model the Dining Philosophers problem. We'll use Haskell with the Parsec library to implement a clean and maintainable parser.

## Table of Contents

1. [Project Setup](#project-setup)
2. [Understanding the Language Structure](#understanding-the-language-structure)
3. [Defining the Abstract Syntax Tree](#defining-the-abstract-syntax-tree)
4. [Building the Parser](#building-the-parser)
5. [Main Module Implementation](#main-module-implementation)
6. [Testing the Parser](#testing-the-parser)
7. [Example: Parsing the Dining Philosophers Program](#example-parsing-the-dining-philosophers-program)

## Project Setup

We'll set up a Haskell project using Stack, which will handle dependencies and building for us.

### Creating the Project Structure

First, let's create a new Stack project:

```bash
stack new Diner
cd Diner
```

### Configuring the Project

Edit your `package.yaml` file to include Parsec as a dependency:

```yaml
name:                Diner
version:             0.1.0.0
github:              "githubuser/Diner"
license:             BSD-3-Clause
author:              "Author name here"
maintainer:          "example@example.com"
copyright:           "2025 Author name here"

extra-source-files:
- README.md
- CHANGELOG.md

description:         Please see the README on GitHub at <https://github.com/githubuser/Diner#readme>

dependencies:
- base >= 4.7 && < 5
- parsec

ghc-options:
- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wmissing-export-lists
- -Wmissing-home-modules
- -Wpartial-fields
- -Wredundant-constraints

library:
  source-dirs: src

executables:
  Diner-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - Diner

tests:
  Diner-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - Diner
```

The key part here is including `parsec` in our dependencies. Parsec is a parser combinator library that allows us to build complex parsers from simple building blocks.

## Understanding the Language Structure

Before we begin writing the parser, let's understand the structure of our DSL. Here's a sample program in our language:

```
// Dining Philosophers Problem
// This demonstrates resource allocation and deadlock prevention

// Define number of philosophers
let numPhils = 5;

// Declare fork resources
foreach 0 to 4 as i {
  declareResource "fork" ++ i;
};

// Create philosophers
foreach 0 to 4 as i {
  // Define fork indices
  let leftFork = "fork" ++ i;
  let rightFork = "fork" ++ (i + 1) % numPhils;
  
  // Spawn philosopher process
  spawn "philosopher" ++ i {
    loop {
      // Think for a random time
      print "Philosopher " ++ i ++ " thinking";
      think (rand 100 500);
      
      // Try to acquire forks
      print "Philosopher " ++ i ++ " hungry, trying to get forks";
      
      lockAll [leftFork, rightFork] [left, right];
      
      // Eat using the acquired forks
      print "Philosopher " ++ i ++ " eating";
      eat (rand 200 400) resource left resource right;
      
      // Release forks
      print "Philosopher " ++ i ++ " finished eating, releasing forks";
      unlockAll [left, right];
    };
  };
};
```

From this example, we can identify language features including:
- Variable declarations with `let`
- Loops with `loop` and `foreach`
- Process spawning with `spawn`
- Resource declaration and management
- Expressions (string concatenation, arithmetic)
- Comments

## Defining the Abstract Syntax Tree

First, we'll define the Abstract Syntax Tree (AST) that represents our language's structure. Create a file `src/Parser.hs`:

```haskell
module Parser where

import Text.Parsec
import Data.List (intercalate)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)

-- Abstract Syntax Tree definitions
data Program = Program [Statement]
  deriving (Eq)

data Resource = Resource String
  deriving (Eq)

data Expr
  = Var String                  -- Variable reference
  | StringLit String            -- String literal
  | Concat Expr Expr            -- String concatenation
  | IntLit Int                  -- Integer literal
  | Rand Expr Expr              -- Random integer in range
  | Add Expr Expr               -- Addition
  | Sub Expr Expr               -- Subtraction
  | Mod Expr Expr               -- Modulo operation
  deriving (Eq)

data Statement
  = Think Expr                             -- Think for computed time units
  | Eat Expr Resource Resource             -- Eat for computed time units using two resources
  | Print String                           -- Print to stdout (stores the evaluated string)
  | PrintExpr Expr                         -- Print expression (for parsing, will be converted to Print)
  | DeclareResource Expr                   -- Declare a global mutex resource 
  | Loop [Statement]                       -- Loop indefinitely
  | Spawn Expr [Statement]                 -- Spawn named process with statements
  | LockAll [Expr] [String]                -- Atomically lock resources, binding results to variables
  | UnlockAll [Resource]                   -- Atomically unlock multiple resources
  | Let String Expr                        -- Variable binding
  | ForEach Int Int String [Statement]     -- Iterate from start to end, binding index to variable
  | If Expr [Statement] [Statement]        -- Conditional execution
  deriving (Eq)
```

Let's understand each part of this AST:

1. `Program` is the root node, containing a list of statements.
2. `Expr` represents expressions like variables, literals, and operations.
3. `Statement` represents the different statements in our language.
4. `Resource` represents a named resource that can be locked/unlocked.

Now we'll add `Show` instances to help with debugging and displaying the AST:

```haskell
instance Show Program where
  show (Program stmts) = unlines (map show stmts)

instance Show Resource where
  show (Resource name) = "resource " ++ name

instance Show Expr where
  show (Var name) = name
  show (StringLit str) = "\"" ++ str ++ "\""
  show (IntLit n) = show n
  show (Concat e1 e2) = "(" ++ show e1 ++ " ++ " ++ show e2 ++ ")"
  show (Rand e1 e2) = "rand " ++ show e1 ++ " " ++ show e2
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mod e1 e2) = "(" ++ show e1 ++ " % " ++ show e2 ++ ")"

instance Show Statement where
  show (Think e) = "think " ++ show e ++ ";"
  show (Eat e r1 r2) = "eat " ++ show e ++ " " ++ show r1 ++ " " ++ show r2 ++ ";"
  show (Print str) = "print \"" ++ str ++ "\";"
  show (PrintExpr e) = "print " ++ show e ++ ";"
  show (DeclareResource e) = "declareResource " ++ show e ++ ";"
  show (Loop stmts) = "loop {\n" ++ indent (unlines (map show stmts)) ++ "};"
  show (Spawn e stmts) = "spawn " ++ show e ++ " {\n" ++ indent (unlines (map show stmts)) ++ "};"
  show (LockAll exprs vars) = "lockAll [" ++ commaSep' (map show exprs) ++ "] [" ++ commaSep' vars ++ "];"
  show (UnlockAll res) = "unlockAll [" ++ commaSep' (map show res) ++ "];"
  show (Let var e) = "let " ++ var ++ " = " ++ show e ++ ";"
  show (ForEach start end var stmts) = 
    "foreach " ++ show start ++ " to " ++ show end ++ " as " ++ var ++ " {\n" ++ 
    indent (unlines (map show stmts)) ++ "};"
  show (If cond thenStmts elseStmts) = 
    "if " ++ show cond ++ " {\n" ++ 
    indent (unlines (map show thenStmts)) ++ 
    (if null elseStmts then "}" else "} else {\n" ++ indent (unlines (map show elseStmts)) ++ "}") ++ ";"

-- Helper functions
indent :: String -> String
indent = unlines . map ("  " ++) . lines

commaSep' :: [String] -> String
commaSep' = intercalate ", "
```

These `Show` instances will allow us to pretty-print the AST, which is helpful for debugging and testing.

## Building the Parser

Now we'll implement the parser itself. We'll use Parsec's lexer to handle common parsing tasks like whitespace and comments.

### Lexer Definition

First, let's define our lexer:

```haskell
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentLine = "//"  -- Define line comments to start with "//"
      , Token.reservedNames =     -- Define keywords that cannot be used as identifiers
          [ "think", "eat", "print", "declareResource", "loop"
          , "spawn", "lockAll", "unlockAll", "let", "foreach"
          , "to", "as", "if", "else", "rand", "resource"
          ]
      , Token.reservedOpNames =   -- Define operators for the language
          [ "+", "-", "%", "++", "=", "=="
          ]
      }

-- Parser utilities from the lexer
-- | Parse a valid identifier (variable name) in our dining philosophers language
-- Identifiers cannot be any of the reserved keywords listed above
identifier :: Parser String
identifier = Token.identifier lexer

-- | Parse one of our language's keywords like "think", "eat", "loop", etc.
-- Fails if the keyword isn't one of those defined in reservedNames
reserved :: String -> Parser ()
reserved = Token.reserved lexer

-- | Parse one of our language's operators: "+", "-", "%", "++", "=", or "=="
-- Fails if the operator isn't one of those defined in reservedOpNames
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- | Parse an integer literal in our language and convert it to Int
-- Used for durations, indices, and numeric values
integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

-- | Parse a string literal enclosed in double quotes
-- Used for messages in print statements and resource names
stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

-- | Parse an expression enclosed in parentheses
-- Used for grouping expressions to control evaluation order
parens :: Parser a -> Parser a
parens = Token.parens lexer

-- | Parse a block of statements enclosed in curly braces
-- Used for loop bodies, spawn blocks, if/else blocks, and foreach loops
braces :: Parser a -> Parser a
braces = Token.braces lexer

-- | Parse a list enclosed in square brackets
-- Used for resource lists in lockAll and unlockAll statements
brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

-- | Parse a semicolon that terminates statements in our language
-- All statements end with an optional semicolon
semi :: Parser String
semi = Token.semi lexer

-- | Parse and consume whitespace and comments (starting with "//")
-- Called at program start and handled automatically between tokens
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- | Parse a comma-separated list of items
-- Used for resource lists in lockAll and unlockAll statements
commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer
```

The lexer will handle:
- Line comments starting with `//`
- Reserved words like `think`, `eat`, etc.
- Reserved operators like `+`, `-`, `%`, etc.
- Whitespace and semicolons

### Expression Parser

Next, we'll implement the parser for expressions:

```haskell
-- Expression parser
term :: Parser Expr
term = parens expr
    <|> Var <$> identifier
    <|> StringLit <$> stringLiteral
    <|> IntLit <$> integer
    <|> randExpr

randExpr :: Parser Expr
randExpr = do
    reserved "rand"
    lowerBound <- expr
    upperBound <- expr
    return $ Rand lowerBound upperBound

expr :: Parser Expr
expr = buildExpressionParser operators term
  where
    operators = 
      [ [Infix (reservedOp "++" >> return Concat) AssocLeft]
      , [Infix (reservedOp "+" >> return Add) AssocLeft,
         Infix (reservedOp "-" >> return Sub) AssocLeft]
      , [Infix (reservedOp "%" >> return Mod) AssocLeft]
      ]
```

Let's break down what's happening here:

1. `term` parses basic expressions like variables, literals, and parenthesized expressions.
2. `randExpr` parses a random expression like `rand 1 10`.
3. `expr` uses `buildExpressionParser` to handle operator precedence:
   - String concatenation (`++`) has the highest precedence
   - Addition (`+`) and subtraction (`-`) have the next highest precedence
   - Modulo (`%`) has the lowest precedence

### Statement Parsers

Now let's implement parsers for each type of statement:

```haskell
-- Statement parsers
statement :: Parser Statement
statement = choice 
    [ thinkStmt
    , eatStmt
    , printStmt
    , declareResourceStmt
    , loopStmt
    , spawnStmt
    , lockAllStmt
    , unlockAllStmt
    , letStmt
    , foreachStmt
    , ifStmt
    ] <* optional semi

thinkStmt :: Parser Statement
thinkStmt = do
    reserved "think"
    Think <$> expr

eatStmt :: Parser Statement
eatStmt = do
    reserved "eat"
    duration <- expr
    reserved "resource"
    resource1Name <- identifier
    reserved "resource"
    resource2Name <- identifier
    return $ Eat duration (Resource resource1Name) (Resource resource2Name)

printStmt :: Parser Statement
printStmt = do
    reserved "print"
    -- Now we can parse any expression and handle string conversion during evaluation
    PrintExpr <$> expr

declareResourceStmt :: Parser Statement
declareResourceStmt = do
    reserved "declareResource"
    DeclareResource <$> expr 

loopStmt :: Parser Statement
loopStmt = do
    reserved "loop"
    Loop <$> braces (many statement)

spawnStmt :: Parser Statement
spawnStmt = do
    reserved "spawn"
    processName <- expr
    body <- braces (many statement)
    return $ Spawn processName body

lockAllStmt :: Parser Statement
lockAllStmt = do
    reserved "lockAll"
    resources <- brackets (commaSep exprNoSpace)
    variables <- brackets (commaSep identifier)
    return $ LockAll resources variables
  where
    -- Define operators locally for this parser
    exprNoSpace = buildExpressionParser operatorsNoSpace termNoSpace
    -- Define the operators table for exprNoSpace
    operatorsNoSpace = 
      [ [Infix (reservedOp "++" >> return Concat) AssocLeft]
      , [Infix (reservedOp "+" >> return Add) AssocLeft,
         Infix (reservedOp "-" >> return Sub) AssocLeft]
      , [Infix (reservedOp "%" >> return Mod) AssocLeft]
      ]
    termNoSpace = parens expr
               <|> Var <$> identifier
               <|> StringLit <$> stringLiteral
               <|> IntLit <$> integer
               <|> randExpr

unlockAllStmt :: Parser Statement
unlockAllStmt = do
    reserved "unlockAll"
    resources <- brackets $ commaSep resourceReference
    return $ UnlockAll resources
  where
    resourceReference = try (do
        reserved "resource"
        Resource <$> identifier)
      <|> (Resource <$> identifier)

letStmt :: Parser Statement
letStmt = do
    reserved "let"
    name <- identifier
    reservedOp "="
    value <- expr
    return $ Let name value

foreachStmt :: Parser Statement
foreachStmt = do
    reserved "foreach"
    startVal <- integer
    reserved "to"
    endVal <- integer
    reserved "as"
    indexVar <- identifier
    body <- braces (many statement)
    return $ ForEach startVal endVal indexVar body

ifStmt :: Parser Statement
ifStmt = do
    reserved "if"
    condition <- expr
    thenBlock <- braces (many statement)
    elseBlock <- option [] (reserved "else" >> braces (many statement))
    return $ If condition thenBlock elseBlock
```

Each statement parser follows a similar pattern:
1. Match the reserved keyword that starts the statement
2. Parse the arguments specific to that statement type
3. Return the corresponding AST node

Let's examine a few specific cases:

1. `lockAllStmt` is interesting because it handles parsing a list of expressions and a list of variables:
   ```haskell
   lockAllStmt :: Parser Statement
   lockAllStmt = do
       reserved "lockAll"
       resources <- brackets (commaSep exprNoSpace)
       variables <- brackets (commaSep identifier)
       return $ LockAll resources variables
   ```

2. `unlockAllStmt` shows how to handle optional keywords:
   ```haskell
   unlockAllStmt :: Parser Statement
   unlockAllStmt = do
       reserved "unlockAll"
       resources <- brackets $ commaSep resourceReference
       return $ UnlockAll resources
     where
       resourceReference = try (do
           reserved "resource"
           Resource <$> identifier)
         <|> (Resource <$> identifier)
   ```

3. `foreachStmt` demonstrates parsing a structured control flow statement:
   ```haskell
   foreachStmt :: Parser Statement
   foreachStmt = do
       reserved "foreach"
       startVal <- integer
       reserved "to"
       endVal <- integer
       reserved "as"
       indexVar <- identifier
       body <- braces (many statement)
       return $ ForEach startVal endVal indexVar body
   ```

### Program Parser

Finally, we'll implement the top-level program parser:

```haskell
-- Program parser
programParser :: Parser Program
programParser = do
    whiteSpace
    stmts <- many statement
    eof
    return $ Program stmts

-- Parse a string into a Program
parseProgram :: String -> Either ParseError Program
parseProgram = parse programParser ""
```

The `programParser` handles whitespace, parses a sequence of statements, and ensures we've reached the end of the file. The `parseProgram` function is a convenience wrapper that applies the parser to a string.

## Main Module Implementation

Now let's implement the Main module to read and parse files. Create a file `app/Main.hs`:

```haskell
module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec.Error (ParseError)
import Parser 

-- | Main function to parse a file
main :: IO ()
main = parseFile 

parseFile :: IO ()
parseFile = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      case parseProgram contents of
        Left err -> do
          putStrLn "Parse error:"
          print err
        Right ast -> do
          putStrLn "Successfully parsed program:"
          print ast
    _ -> putStrLn "Usage: parser <filename>"
```

The `main` function:
1. Gets command-line arguments
2. Reads the file content
3. Tries to parse the content using our `parseProgram` function
4. Prints either the error or the resulting AST

## Testing the Parser

Let's add some test functions to our Main module to help us verify our parser:

```haskell
-- | Parse a string and print the result
parseAndPrint :: String -> IO ()
parseAndPrint input = do
  putStrLn "Input:"
  putStrLn input
  putStrLn ""
  putStrLn "Result:"
  case parseProgram input of
    Left err -> print err
    Right ast -> print ast

-- Example programs for testing
examplePrograms :: [(String, String)]
examplePrograms =
  [ ("Simple Variable Declaration", "let x = 42;")
  
  , ("Basic Operations", unlines
      [ "let a = 10;"
      , "let b = 20;"
      , "let sum = a + b;"
      , "let diff = a - b;"
      , "let mod = a % b;"
      ])
  
  , ("String Operations", unlines
      [ "let name = \"Alice\";"
      , "let greeting = \"Hello, \" ++ name;"
      , "print greeting;"
      ])
  
  , ("Control Flow", unlines
      [ "let x = 10;"
      , "if x % 2 {"
      , "  print \"x is odd\";"
      , "} else {"
      , "  print \"x is even\";"
      , "};"
      ])
  
  , ("Resource Management", unlines
      [ "declareResource \"resource1\";"
      , "declareResource \"resource2\";"
      , "lockAll [resource1, resource2] [r1, r2];"
      , "think 10;"
      , "unlockAll [r1, r2];"
      ])
  
  , ("Process Spawning", unlines
      [ "spawn \"worker\" {"
      , "  loop {"
      , "    think (rand 10 50);"
      , "    print \"Worker is working\";"
      , "  };"
      , "};"
      ])
  
  , ("Iteration", unlines
      [ "let sum = 0;"
      , "foreach 1 to 10 as i {"
      , "  let sum = sum + i;"
      , "};"
      , "print \"Sum: \" ++ sum;"
      ])
  ]

-- | Function to run all example programs
runExamples :: IO ()
runExamples = do
  putStrLn "Running example programs:"
  mapM_ (\(name, prog) -> do
    putStrLn $ "\n=== " ++ name ++ " ==="
    parseAndPrint prog
    ) examplePrograms

-- Simple unit test framework
testParser :: IO ()
testParser = do
  putStrLn "Running parser tests..."
  
  -- Test expressions
  testExpr "42" "IntLit 42"
  testExpr "x" "Var \"x\""
  testExpr "\"hello\"" "StringLit \"hello\""
  testExpr "a + b" "Add (Var \"a\") (Var \"b\")"
  testExpr "a - b" "Sub (Var \"a\") (Var \"b\")"
  testExpr "a % b" "Mod (Var \"a\") (Var \"b\")"
  testExpr "\"a\" ++ \"b\"" "Concat (StringLit \"a\") (StringLit \"b\")"
  testExpr "rand 1 10" "Rand (IntLit 1) (IntLit 10)"
  
  -- Test statements
  testStmt "think 42;" "Think (IntLit 42)"
  testStmt "print \"hello\";" "Print \"hello\""
  testStmt "let x = 42;" "Let \"x\" (IntLit 42)"
  
  -- More complex tests
  testProgram "let x = 42; think x;" 
              "Program [Let \"x\" (IntLit 42),Think (Var \"x\")]"
  
  putStrLn "All tests completed."

  where
    testExpr input expected = do
      case parseProgram ("let test = " ++ input ++ ";") of
        Left err -> putStrLn $ "FAIL: Could not parse expression: " ++ input ++ "\nError: " ++ show err
        Right (Program [Let _ expr]) -> 
          if show expr == expected
            then putStrLn $ "PASS: " ++ input
            else putStrLn $ "FAIL: " ++ input ++ "\nExpected: " ++ expected ++ "\nGot: " ++ show expr
        Right other -> putStrLn $ "FAIL: Unexpected parse result for " ++ input ++ ": " ++ show other
    
    testStmt input expected = do
      case parseProgram input of
        Left err -> putStrLn $ "FAIL: Could not parse statement: " ++ input ++ "\nError: " ++ show err
        Right (Program [stmt]) -> 
          if show stmt == expected
            then putStrLn $ "PASS: " ++ input
            else putStrLn $ "FAIL: " ++ input ++ "\nExpected: " ++ expected ++ "\nGot: " ++ show stmt
        Right other -> putStrLn $ "FAIL: Unexpected parse result for " ++ input ++ ": " ++ show other
    
    testProgram input expected = do
      case parseProgram input of
        Left err -> putStrLn $ "FAIL: Could not parse program: " ++ input ++ "\nError: " ++ show err
        Right program -> 
          if show program == expected
            then putStrLn $ "PASS: " ++ input
            else putStrLn $ "FAIL: " ++ input ++ "\nExpected: " ++ expected ++ "\nGot: " ++ show program
```

This adds several test functions:
1. `parseAndPrint` - parses a string and prints the result
2. `examplePrograms` - a list of example programs to test
3. `runExamples` - runs all the example programs
4. `testParser` - a simple unit testing framework

## Example: Parsing the Dining Philosophers Program

Let's see how our parser handles the Dining Philosophers example. Create a file called `Dining.din` with the content from our earlier example.

To run the parser:

```bash
stack build
stack run Dining.din
```

This should output the AST representation of the program. Let's analyze how a few key parts would be parsed:

1. **Resource Declaration**: The following code:
   ```
   declareResource "fork" ++ i;
   ```
   Will be parsed as:
   ```
   DeclareResource (Concat (StringLit "fork") (Var "i"))
   ```

2. **Spawn Process**: This spawn statement:
   ```
   spawn "philosopher" ++ i { ... }
   ```
   Will be parsed as:
   ```
   Spawn (Concat (StringLit "philosopher") (Var "i")) [...]
   ```

3. **For Loop**: The following foreach loop:
   ```
   foreach 0 to 4 as i { ... }
   ```
   Will be parsed as:
   ```
   ForEach 0 4 "i" [...]
   ```

4. **Resource Locking**: The lock statement:
   ```
   lockAll [leftFork, rightFork] [left, right];
   ```
   Will be parsed as:
   ```
   LockAll [Var "leftFork", Var "rightFork"] ["left", "right"]
   ```

5. **Eating with Resources**: The eat statement:
   ```
   eat (rand 200 400) resource left resource right;
   ```
   Will be parsed as:
   ```
   Eat (Rand (IntLit 200) (IntLit 400)) (Resource "left") (Resource "right")
   ```

## Conclusion

In this tutorial, we've built a parser for a domain-specific language to model the Dining Philosophers problem. We used Haskell's Parsec library to create a clean, maintainable parser. The key steps were:

1. Define the Abstract Syntax Tree (AST) to represent the language
2. Create a lexer to handle basic parsing tasks
3. Implement parsers for expressions and statements
4. Combine these parsers to create a complete program parser
5. Add testing functions to verify our parser works correctly

This parser can now be integrated with an interpreter or compiler to execute programs in our DSL. The Dining Philosophers example demonstrates how our language can model concurrent systems with resource allocation.

For further development, you might consider:
- Adding type checking
- Implementing an interpreter to execute programs
- Adding more language features or optimizations
- Creating visualizations of the execution

Happy programming!

# More on Lexers 
A lexer is a fundamental component in the parsing process of a programming language or domain-specific language. 

## What is a Lexer?

A lexer (short for "lexical analyzer") is the first stage in the process of interpreting or compiling code. It has a specific job: to transform a stream of characters (the raw text of your program) into a stream of "tokens."

### Key Functions of a Lexer:

1. **Tokenization**: Breaking down the input text into meaningful chunks called tokens
2. **Classification**: Identifying what type each token is (keyword, identifier, literal, operator, etc.)
3. **Filtering**: Removing elements not needed for parsing, such as whitespace and comments
4. **Position tracking**: Recording line and column information for error reporting

### Example of Lexical Analysis:

Consider this line from your Dining language:
```
let leftFork = "fork" ++ i;
```

The lexer would break this down into tokens like:
- `let` (keyword)
- `leftFork` (identifier)
- `=` (operator)
- `"fork"` (string literal)
- `++` (operator)
- `i` (identifier)
- `;` (semicolon)

## Lexer in Our Implementation

In our Haskell implementation, we're using Parsec's token parser to create our lexer:

```haskell
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentLine = "//"
      , Token.reservedNames = 
          [ "think", "eat", "print", "declareResource", "loop"
          , "spawn", "lockAll", "unlockAll", "let", "foreach"
          , "to", "as", "if", "else", "rand", "resource"
          ]
      , Token.reservedOpNames = 
          [ "+", "-", "%", "++", "=", "=="
          ]
      }
```

Let's break this down:

1. `Token.makeTokenParser` creates a lexer based on a language definition
2. `emptyDef` provides a basic language definition that we customize with:
   - `commentLine = "//"` - Defines how comments start in our language
   - `reservedNames` - Keywords in our language that can't be used as identifiers
   - `reservedOpNames` - Operators in our language

From this lexer, we derive several helper parsers:

```haskell
identifier :: Parser String       -- Parses variable names
reserved :: String -> Parser ()   -- Parses keywords
reservedOp :: String -> Parser () -- Parses operators
integer :: Parser Int             -- Parses integer literals
stringLiteral :: Parser String    -- Parses string literals
parens :: Parser a -> Parser a    -- Parses parenthesized expressions
braces :: Parser a -> Parser a    -- Parses braced blocks
brackets :: Parser a -> Parser a  -- Parses bracketed lists
semi :: Parser String             -- Parses semicolons
whiteSpace :: Parser ()           -- Parses whitespace
commaSep :: Parser a -> Parser [a] -- Parses comma-separated lists
```

These functions handle the low-level details of parsing different token types, allowing our higher-level parsers to focus on the structure of the language rather than the character-by-character parsing.

## Why Lexers Are Important

1. **Simplification**: They simplify the parser's job by handling low-level character processing
2. **Error Handling**: They provide helpful error messages at the lexical level
3. **Efficiency**: They can make parsing more efficient by doing the character-by-character work once
4. **Separation of Concerns**: They maintain a clean separation between lexical analysis and syntactic analysis

## Lexer vs. Parser

To clarify the difference:
- **Lexer** (Lexical Analyzer): Converts character stream → token stream
- **Parser** (Syntactic Analyzer): Converts token stream → parse tree/AST

In our implementation, the lexer handles identifying tokens like keywords, operators, and literals, while the parser is responsible for understanding the grammatical structure of the program and building the AST.

For example, in the expression parser:

```haskell
expr :: Parser Expr
expr = buildExpressionParser operators term
```

The lexer has already identified tokens like variables and operators, and the parser is using them to build a structured representation of expressions with proper operator precedence.
