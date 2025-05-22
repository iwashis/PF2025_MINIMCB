# Building a Test Suite for the Dining Philosophers Parser

This tutorial continues from our parser building tutorial, where we created a parser for the Dining Philosophers language. Now we'll add a comprehensive test suite using Haskell's testing frameworks to ensure our parser works correctly and catches regressions as we develop.

## Table of Contents

1. [Project Setup and Dependencies](#project-setup-and-dependencies)
2. [Understanding Haskell Testing Frameworks](#understanding-haskell-testing-frameworks)
3. [Setting Up the Test Structure](#setting-up-the-test-structure)
4. [Writing Unit Tests](#writing-unit-tests)
5. [Property-Based Testing with QuickCheck](#property-based-testing-with-quickcheck)
6. [Running and Interpreting Tests](#running-and-interpreting-tests)
7. [Best Practices for Parser Testing](#best-practices-for-parser-testing)

## Project Setup and Dependencies

First, we need to update our `package.yaml` file to include the necessary testing dependencies and configure our test suite properly.

### Updating package.yaml

Let's understand the testing dependencies we've added:

1. **HUnit**: A unit testing framework for Haskell, similar to JUnit for Java. It's used for writing explicit test cases with expected inputs and outputs.

2. **QuickCheck**: A property-based testing framework that automatically generates test cases. Instead of writing specific examples, you write properties that should hold for any valid input.

3. **test-framework**: A unified interface that allows you to run both HUnit and QuickCheck tests together.

4. **test-framework-hunit**: Adapter to run HUnit tests within the test-framework.

5. **test-framework-quickcheck2**: Adapter to run QuickCheck tests within the test-framework.

### Key Changes Explained

In the `tests` section:
- `main: Spec.hs` - This is our main test runner file
- `source-dirs: test` - Tests will be in the `test/` directory
- The dependencies include our library (`Diner`) plus all testing frameworks
- We use the same GHC options for consistency and performance

## Understanding Haskell Testing Frameworks

Before diving into implementation, let's understand the different testing approaches:

### Unit Testing with HUnit

Unit tests verify that specific inputs produce expected outputs. They're great for:
- Testing edge cases
- Verifying specific parsing scenarios
- Ensuring error handling works correctly

Example unit test structure:
```haskell
test_parseInteger :: Assertion
test_parseInteger = parseTest parseProgram "let x = 42;" (Program [Let "x" (IntLit 42)])
```

### Property-Based Testing with QuickCheck

Property-based tests verify that certain properties hold for a wide range of generated inputs. They're excellent for:
- Finding edge cases you didn't think of
- Ensuring parsing is consistent
- Testing round-trip properties (parse then pretty-print)

Example property test:
```haskell
prop_integerRoundTrip :: Int -> Property
prop_integerRoundTrip n = parseProgram (show n) === Right (IntLit n)
```

## Setting Up the Test Structure

Now let's create the test directory structure and implement our test suite.

### Main Test Runner

First, create `test/Spec.hs` - this is our test entry point:

This simple module:
1. Imports `defaultMain` from the test framework, which handles running tests and reporting results
2. Imports our specific test module `Parser.ParserSpec`
3. Runs all tests defined in `Parser.ParserSpec.tests`

The `defaultMain` function automatically:
- Runs all tests
- Reports which tests pass/fail
- Provides timing information
- Handles command-line options for filtering tests

### Test Module Structure

Now let's create `test/Parser/ParserSpec.hs` - our comprehensive test suite:

Let's understand this structure:

1. **Module exports**: We export `tests`, which contains all our test cases
2. **Test organization**: Tests are grouped into "Unit tests" and "Property-based tests"
3. **Helper function**: `parseTest` is a utility that makes writing parser tests easier

The `parseTest` helper function:
- Takes a parser function, input string, and expected result
- Runs the parser on the input
- If parsing fails, it reports the error
- If parsing succeeds, it compares the result with the expected AST using `assertEqual`

## Writing Unit Tests

Unit tests verify specific parsing scenarios. Let's implement several comprehensive unit tests:

Let's examine each unit test:

### 1. Integer Literal Test
```haskell
test_intLit = parseTest parseProgram "let x = 42;" (Program [Let "x" (IntLit 42)])
```
This test verifies that integer literals are parsed correctly into `IntLit` AST nodes within variable declarations.

### 2. String Literal Test
```haskell
test_strLit = parseTest parseProgram "let s = \"hello\";" (Program [Let "s" (StringLit "hello")])
```
This ensures string literals (including the quotes) are properly parsed into `StringLit` nodes.

### 3. Think Statement Test
```haskell
test_think = parseTest parseProgram "think 100;" (Program [Think (IntLit 100)])
```
This tests that the `think` statement, a key primitive in our dining philosophers language, parses correctly.

### 4. If Statement Test
This is a more complex test that verifies:
- Conditional expressions (`x % 2`)
- Block structure with braces
- Both `then` and `else` branches
- Nested statements within blocks

### 5. ForEach Statement Test
This tests our iteration construct, ensuring:
- Range specification (`1 to 5`)
- Variable binding (`as i`)
- Statement blocks within the loop body

## Property-Based Testing with QuickCheck

Now let's add property-based tests that generate many test cases automatically. First, we need to create generators for our data types:

Let's understand these generators:

### 1. Identifier Generator
```haskell
genIdentifier :: Gen String
genIdentifier = do
    first <- elements $ ['a' .. 'z'] 
    rest <- listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']
    return (first : take 10 rest)
```

This generator creates valid variable names:
- First character must be lowercase (following common conventions)
- Subsequent characters can be letters, numbers, or underscores
- Length is limited to 10 characters for readability

### 2. String Literal Generator
```haskell
genStringLit :: Gen String
genStringLit =
    resize 20 $
        listOf $
            elements $
                ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' ', '!', '?', '.', ',']
```

This creates strings with safe characters (avoiding quotes and control characters that could break parsing).

### 3. Expression Generator with Controlled Recursion
```haskell
genExpr :: Int -> Gen Expr
genExpr 0 = oneof [IntLit <$> genSmallInt, StringLit <$> genStringLit, Var <$> genIdentifier]
genExpr n | n > 0 = frequency [...expressions with subexpressions...]
```

This generator:
- Uses a depth parameter to control recursion
- At depth 0, only generates leaf expressions (literals and variables)
- At higher depths, can generate compound expressions
- Uses `frequency` to weight simpler expressions more heavily than complex ones

Now let's add the actual property tests:

Let's examine each property test:

### 1. Integer Literal Property
```haskell
prop_intLit :: Int -> Property
prop_intLit n = parseProgram src === Right prog
```
This property states: "For any integer n, parsing a let statement with that integer should produce the expected AST." QuickCheck will test this with many different integers, including edge cases like very large numbers, negative numbers, and zero.

### 2. String Literal Property
```haskell
prop_strLit = forAll genStringLit $ \s -> parseProgram src === Right prog
```
This ensures that any string generated by our `genStringLit` generator can be parsed correctly. The `forAll` explicitly uses our custom generator instead of QuickCheck's default string generator.

### 3. Variable Binding Property
```haskell
prop_varBinding = forAll genIdentifier $ \varName ->
    forAll (genExpr 1) $ \expr -> ...
```
This is more complex - it tests that any valid identifier can be bound to any simple expression. This catches issues with reserved words, special characters, or expression parsing edge cases.

### 4. Random Expression Property
```haskell
prop_randomExpr = forAll (genExpr 2) $ \expr ->
    case parseProgram src of
        Right _ -> True
        Left _ -> False
```
This property tests that any expression we can generate can be parsed successfully. It's a "smoke test" that ensures our expression generator and parser are compatible.

### 5. Whitespace Insignificance Property
```haskell
prop_whitespace = parseProgram compact === parseProgram spaced
```
This verifies that extra whitespace doesn't affect parsing results - a crucial property for any parser.

Finally, let's add the test group definitions:

These test groups organize our tests and provide descriptive names that will appear in the test output.

## Running and Interpreting Tests

Now that we have our complete test suite, let's understand how to run and interpret the results.

### Building and Running Tests

To run the tests, use these Stack commands:

```bash
# First, make sure everything builds
stack build

# Run the test suite
stack test

# Run tests with more verbose output
stack test --test-arguments="--maximum-generated-tests=1000"

# Run only specific test groups
stack test --test-arguments="--select-tests=Unit"
```

### Understanding Test Output

When you run the tests, you'll see output like this:

```
Unit tests:
  Parse integer literal:     OK
  Parse string literal:      OK  
  Parse think statement:     OK
  Parse if statement:        OK
  Parse foreach statement:   OK

Property-based tests:
  Integer literals parse correctly:     OK, passed 100 tests.
  String literals parse correctly:      OK, passed 100 tests.
  Variable binding can be parsed:       OK, passed 100 tests.
  Random expressions can be parsed:     OK, passed 100 tests.
  Whitespace is insignificant:         OK, passed 100 tests.

Properties  Test Cases  Total
     5           5         10  
     5           5         10  

All 10 tests passed.
```

### Interpreting Failures

If a test fails, you'll see detailed information:

**Unit Test Failure:**
```
Parse if statement: FAIL
Expected: Program [If (Mod (Var "x") (IntLit 2)) ...]
Actual:   Program [If (Mod (Var "x") (IntLit 2)) ...]
```

**Property Test Failure:**
```
Variable binding can be parsed: FAIL
*** Failed! (after 23 tests):
"x1_"
Add (IntLit 42) (StringLit "test")
```

This shows the specific inputs that caused the failure, helping you debug the issue.

### Coverage Analysis

You can also analyze test coverage to ensure you're testing all parts of your parser:

```bash
# Generate coverage report
stack test --coverage

# View the coverage report
stack hpc report Diner-test
```

This will show which parts of your parser code are being exercised by the tests.

## Best Practices for Parser Testing

### 1. Test Edge Cases Explicitly
Always include unit tests for edge cases that might not be caught by property tests:

```haskell
test_emptyString :: Assertion
test_emptyString = parseTest parseProgram "let x = \"\";" (Program [Let "x" (StringLit "")])

test_zeroValue :: Assertion 
test_zeroValue = parseTest parseProgram "let x = 0;" (Program [Let "x" (IntLit 0)])

test_negativeNumbers :: Assertion
test_negativeNumbers = parseTest parseProgram "let x = -42;" (Program [Let "x" (IntLit (-42))])
```

### 2. Test Error Cases
Don't just test successful parsing - also test that invalid input produces appropriate errors:

```haskell
test_invalidSyntax :: Assertion
test_invalidSyntax = 
    case parseProgram "let x =" of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Should have failed to parse incomplete let statement"
```

### 3. Use Descriptive Test Names
Good test names make it clear what's being tested:

```haskell
-- Good
test_parseComplexExpression :: Assertion
test_parseNestedIfStatements :: Assertion
test_parseResourceLockingWithMultipleResources :: Assertion

-- Bad
test1 :: Assertion
test_parse :: Assertion
test_stuff :: Assertion
```

### 4. Group Related Tests
Organize tests logically using `testGroup`:

```haskell
expressionTests = testGroup "Expression parsing"
    [ testCase "Integer literals" test_intLit
    , testCase "String concatenation" test_concat
    , testCase "Arithmetic operations" test_arithmetic
    ]

statementTests = testGroup "Statement parsing"
    [ testCase "Think statements" test_think
    , testCase "If statements" test_if
    , testCase "Loop statements" test_loop
    ]
```

### 5. Balance Unit and Property Tests
- Use unit tests for specific, critical cases
- Use property tests for general correctness across many inputs
- Property tests often find edge cases you didn't think of
- Unit tests provide clear regression testing

### 6. Make Tests Fast
Keep tests fast so they can be run frequently during development:
- Limit generated test case sizes
- Use small input ranges for numeric generators
- Avoid testing very deep recursive structures

## Advanced Testing Techniques

### Round-Trip Testing
Test that parsing and pretty-printing are inverse operations:

```haskell
prop_roundTrip :: Property
prop_roundTrip = forAll (genExpr 2) $ \expr ->
    case parseProgram (show expr) of
        Right (Program [Let _ parsedExpr]) -> parsedExpr == expr
        _ -> False
```

### Testing with Real Examples
Include tests with realistic programs:

```haskell
test_diningPhilosophersSnippet :: Assertion
test_diningPhilosophersSnippet = do
    let program = unlines
            [ "let numPhils = 5;"
            , "foreach 0 to 4 as i {"
            , "  declareResource \"fork\" ++ i;"
            , "};"
            ]
    case parseProgram program of
        Right _ -> return ()
        Left err -> assertFailure $ "Failed to parse dining philosophers snippet: " ++ show err
```

## Conclusion

We've now built a comprehensive test suite for our Dining Philosophers parser that includes:

1. **Project configuration** with proper testing dependencies
2. **Unit tests** for specific parsing scenarios and edge cases
3. **Property-based tests** that generate many test cases automatically
4. **Organized test structure** that's easy to maintain and extend
5. **Best practices** for parser testing

This test suite will:
- Catch regressions when you modify the parser
- Help you confidently add new language features
- Document the expected behavior of your parser
- Find edge cases you might not have considered

Key benefits of this approach:
- **Quick feedback** during development
- **Confidence** when refactoring
- **Documentation** of parser behavior
- **Regression prevention** as the project grows

To continue developing your parser:
1. Run tests frequently with `stack test`
2. Add new tests when you add language features
3. Use property tests to explore the behavior of new constructs
4. Keep tests fast and focused

The next step in this project series would be building an interpreter or evaluator for the parsed AST, which would benefit greatly from having this solid testing foundation in place.
