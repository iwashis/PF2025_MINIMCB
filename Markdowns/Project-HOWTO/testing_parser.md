# Testing the Dining Philosophers Language Parser

This tutorial will guide you through creating a comprehensive testing suite for your Dining Philosophers language parser. We'll focus on two complementary testing approaches: unit tests for specific functionality and property-based tests for broader correctness properties.

## Table of Contents

1. [Understanding Parser Testing](#understanding-parser-testing)
2. [Project Setup for Testing](#project-setup-for-testing)
3. [Writing the First Unit Tests](#writing-the-first-unit-tests)
4. [Building Property-Based Tests](#building-property-based-tests)
5. [Testing Edge Cases](#testing-edge-cases)
6. [Integration with Stack](#integration-with-stack)
7. [Running the Tests](#running-the-tests)

## Understanding Parser Testing

Testing a parser requires verifying two key aspects:

1. **Correctness**: Does the parser correctly transform input strings into the right Abstract Syntax Tree (AST) structures?
2. **Robustness**: Does the parser properly handle edge cases, errors, and a wide range of valid inputs?

For our Dining Philosophers language parser, we'll use both:

- **Unit tests**: For specific, targeted testing of parser features
- **Property-based tests**: For generating many test cases to find subtle issues

## Project Setup for Testing

To add testing to your Stack project, you'll need to update your `package.yaml` file to include the necessary testing libraries:

```yaml
tests:
  dining-philosophers-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - dining-philosophers
    - HUnit >= 1.6
    - QuickCheck >= 2.14
    - test-framework >= 0.8
    - test-framework-hunit >= 0.3
    - test-framework-quickcheck2 >= 0.3
```

This configuration sets up:
- `HUnit` for unit testing
- `QuickCheck` for property-based testing
- `test-framework` packages to provide a unified testing framework

### Create the Test Directory Structure

First, create the necessary directories and files:

```
mkdir -p test/Parser
touch test/Spec.hs
touch test/Parser/ParserSpec.hs
```

## Writing the First Unit Tests

Let's start by setting up the main test file. Create `test/Spec.hs`:

```haskell
module Main (main) where

import Test.Framework (defaultMain)
import qualified Parser.ParserSpec

main :: IO ()
main = defaultMain Parser.ParserSpec.tests
```

This file is simple: it imports our test modules and runs them.

### Basic Parser Unit Tests

Now, let's create our first unit tests in `test/Parser/ParserSpec.hs`:

```haskell
module Parser.ParserSpec (tests) where

import Test.HUnit
import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Text.Parsec (parse, ParseError)

-- Import the parser module from your library
import Parser

-- | All tests
tests = [
    testGroup "Unit tests" unitTests,
    testGroup "Property-based tests" propertyTests
]

-- | Helper to run a parser on input and check against expected AST
parseTest :: (Show a, Eq a) => (String -> Either ParseError a) -> String -> a -> Assertion
parseTest parser input expected = 
    case parser input of
        Left err -> assertFailure $ "Parse error: " ++ show err
        Right result -> assertEqual ("Parsing: " ++ input) expected result

-- | Unit Tests

-- Test integer literals
test_intLit :: Assertion
test_intLit = parseTest parseProgram "let x = 42;" (Program [Let "x" (IntLit 42)])

-- Test string literals
test_strLit :: Assertion
test_strLit = parseTest parseProgram "let s = \"hello\";" (Program [Let "s" (StringLit "hello")])

-- Test think statement
test_think :: Assertion
test_think = parseTest parseProgram "think 100;" (Program [Think (IntLit 100)])

-- Test if statement
test_ifStatement :: Assertion
test_ifStatement = parseTest parseProgram 
    "if x % 2 { print \"odd\"; } else { print \"even\"; };" 
    (Program [If (Mod (Var "x") (IntLit 2)) 
                [PrintExpr (StringLit "odd")] 
                [PrintExpr (StringLit "even")]])

-- Test foreach statement
test_foreachStatement :: Assertion
test_foreachStatement = parseTest parseProgram 
    "foreach 1 to 5 as i { print i; };" 
    (Program [ForEach 1 5 "i" [PrintExpr (Var "i")]])

-- Unit tests group
unitTests = [
    testCase "Parse integer literal" test_intLit,
    testCase "Parse string literal" test_strLit,
    testCase "Parse think statement" test_think,
    testCase "Parse if statement" test_ifStatement,
    testCase "Parse foreach statement" test_foreachStatement
]

-- Property tests group (placeholder - we'll implement these next)
propertyTests = []
```

This initial module provides:

1. A `parseTest` helper function to simplify testing
2. Five basic unit tests for different language features
3. A test group structure to organize tests

Let's analyze each test:

- `test_intLit`: Tests parsing of integer literals in a variable assignment
- `test_strLit`: Tests parsing of string literals
- `test_think`: Tests the "think" statement, a unique feature of our language
- `test_ifStatement`: Tests conditional statements with both branches
- `test_foreachStatement`: Tests the loop construct with variable binding

## Building Property-Based Tests

Property-based testing with QuickCheck allows us to generate many test cases automatically. Let's add property tests to our `ParserSpec.hs` file:

```haskell
-- | QuickCheck Properties

-- Generator for valid identifiers (variable names)
genIdentifier :: Gen String
genIdentifier = do
    first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
    return (first : take 10 rest)  -- Limit identifier length for readability

-- Generator for simple string literals
genStringLit :: Gen String
genStringLit = resize 20 $ listOf $ elements $ 
    ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '!', '?', '.', ',']

-- Generator for small integers
genSmallInt :: Gen Int
genSmallInt = choose (-100, 100)

-- Generator for simple expressions
genExpr :: Int -> Gen Expr
genExpr 0 = oneof [
        IntLit <$> genSmallInt,
        StringLit <$> genStringLit,
        Var <$> genIdentifier
    ]
genExpr n | n > 0 = 
    let subexpr = genExpr (n - 1)
    in frequency [
        (3, IntLit <$> genSmallInt),
        (3, StringLit <$> genStringLit),
        (3, Var <$> genIdentifier),
        (2, Add <$> subexpr <*> subexpr),
        (1, Mod <$> subexpr <*> subexpr),
        (2, Concat <$> subexpr <*> subexpr)
    ]

-- Property 1: Integer literals parse correctly
prop_intLit :: Int -> Property
prop_intLit n = 
    let 
        expr = IntLit n
        prog = Program [Let "x" expr]
        src = "let x = " ++ show n ++ ";"
    in
        parseProgram src === Right prog

-- Property 2: String literals parse correctly
prop_strLit :: Property
prop_strLit = forAll genStringLit $ \s ->
    let 
        expr = StringLit s
        prog = Program [Let "x" expr]
        src = "let x = \"" ++ s ++ "\";"
    in
        parseProgram src === Right prog

-- Property 3: Variable binding can be parsed
prop_varBinding :: Property
prop_varBinding = forAll genIdentifier $ \varName ->
    forAll (genExpr 1) $ \expr ->
        let 
            stmt = Let varName expr
            prog = Program [stmt]
            src = "let " ++ varName ++ " = " ++ show expr ++ ";"
        in
            case parseProgram src of
                Right p -> prog == p
                Left _ -> False

-- Property 4: Random expressions can be parsed
prop_randomExpr :: Property
prop_randomExpr = forAll (genExpr 2) $ \expr ->
    let src = "let test = " ++ show expr ++ ";"
    in case parseProgram src of
        Right _ -> True
        Left _ -> False

-- Property 5: Whitespace is insignificant
prop_whitespace :: Property
prop_whitespace = forAll genIdentifier $ \varName ->
    forAll genSmallInt $ \value ->
        let 
            compact = "let " ++ varName ++ "=" ++ show value ++ ";"
            spaced = "let  " ++ varName ++ "  =  " ++ show value ++ "  ;"
        in
            parseProgram compact === parseProgram spaced

-- Update the property tests group
propertyTests = [
    testProperty "Integer literals parse correctly" prop_intLit,
    testProperty "String literals parse correctly" prop_strLit,
    testProperty "Variable binding can be parsed" prop_varBinding,
    testProperty "Random expressions can be parsed" prop_randomExpr,
    testProperty "Whitespace is insignificant" prop_whitespace
]
```

Our property-based tests cover:

1. **Generator Functions**: We create generators for identifiers, strings, integers, and expressions.
2. **Roundtrip Testing**: We test that our parser correctly reconstructs the AST from generated code.
3. **Language Properties**: We verify properties of our language, like whitespace insensitivity.

Let's examine each property:

- `prop_intLit`: Tests that any integer literal can be parsed correctly
- `prop_strLit`: Tests that any valid string literal parses correctly
- `prop_varBinding`: Tests variable binding with different names and values
- `prop_randomExpr`: Tests that randomly generated expressions can be parsed
- `prop_whitespace`: Tests that whitespace doesn't affect parsing results

## Testing Edge Cases

Real-world parsers need to handle not just valid inputs but also edge cases. Let's add some helper functions and edge case tests:

```haskell
-- | Helper to verify that parsing fails as expected
parseFailTest :: (String -> Either ParseError a) -> String -> Assertion
parseFailTest parser input =
    case parser input of
        Left _ -> return () -- Expected failure
        Right result -> assertFailure $ "Expected parse failure, but got: " ++ show result

-- | Helper to test that a program can be parsed
canParseTest :: String -> Assertion
canParseTest input =
    case parseProgram input of
        Left err -> assertFailure $ "Parse error: " ++ show err
        Right _ -> return () -- Successfully parsed

-- Test empty program
test_emptyProgram :: Assertion
test_emptyProgram = parseTest parseProgram "" (Program [])

-- Test invalid syntax
test_invalidSyntax :: Assertion
test_invalidSyntax = do
    -- Missing semicolon
    parseFailTest parseProgram "let x = 42"
    -- Invalid expression
    parseFailTest parseProgram "let x = ;"
    -- Mismatched braces
    parseFailTest parseProgram "if x { print 42;"

-- Update the unit tests group to include edge cases
unitTests = [
    testCase "Parse integer literal" test_intLit,
    testCase "Parse string literal" test_strLit,
    testCase "Parse think statement" test_think,
    testCase "Parse if statement" test_ifStatement,
    testCase "Parse foreach statement" test_foreachStatement,
    testCase "Parse empty program" test_emptyProgram,
    testCase "Invalid syntax" test_invalidSyntax
]
```

These edge case tests ensure our parser:

1. Accepts empty programs
2. Properly rejects invalid syntax
3. Produces appropriate error messages

## Integration with Stack

Now that we have our test suite, let's integrate it with Stack to make running tests easy.

### Final Directory Structure

Your project should now have this structure:

```
dining-philosophers/
├── app/
│   └── Main.hs
├── src/
│   └── Parser.hs
├── test/
│   ├── Parser/
│   │   └── ParserSpec.hs
│   └── Spec.hs
├── package.yaml
└── stack.yaml
```

### Update package.yaml

Make sure your `package.yaml` file exposes the Parser module:

```yaml
library:
  source-dirs: src
  exposed-modules:
    - Parser
```

## Running the Tests

With everything set up, you can now run your tests using Stack:

```bash
stack test
```

This will:
1. Build your project
2. Compile the tests
3. Run all tests and report results

You should see output similar to:

```
dining-philosophers-test: 
  Unit tests:
    Parse integer literal:     OK
    Parse string literal:      OK
    Parse think statement:     OK
    Parse if statement:        OK
    Parse foreach statement:   OK
    Parse empty program:       OK
    Invalid syntax:            OK

  Property-based tests:
    Integer literals parse correctly:  OK (0.02s)
      +++ OK, passed 100 tests.
    String literals parse correctly:   OK (0.02s)
      +++ OK, passed 100 tests.
    Variable binding can be parsed:    OK (0.03s)
      +++ OK, passed 100 tests.
    Random expressions can be parsed:  OK (0.04s)
      +++ OK, passed 100 tests.
    Whitespace is insignificant:       OK (0.01s)
      +++ OK, passed 100 tests.

All 12 tests passed (0.12s)
```

### Running Specific Tests

You can run specific test groups or tests:

```bash
# Run only unit tests
stack test --test-arguments="--select-tests Pattern:\"Unit\""

# Run only property tests
stack test --test-arguments="--select-tests Pattern:\"Property\""
```

## Summary

In this tutorial, we've created a comprehensive testing suite for our Dining Philosophers language parser. We've:

1. Set up a Stack project with testing support
2. Created basic unit tests for parser functionality
3. Implemented property-based tests with QuickCheck
4. Added edge case testing
5. Integrated everything with Stack for easy testing

This testing approach ensures our parser correctly handles both specific examples and randomly generated inputs, providing confidence in its correctness and robustness.

## Next Steps

To further enhance your parser testing, consider:

1. **More comprehensive testing**: Add tests for all language features
2. **Stress testing**: Test with very large programs
3. **Error message testing**: Verify that error messages are helpful
4. **Performance testing**: Measure parsing speed for large inputs
5. **Fuzzing**: Use techniques like fuzzing to find parsing bugs

By maintaining a solid test suite, you can continue to evolve your parser with confidence that it will correctly handle the Dining Philosophers language.
