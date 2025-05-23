module Parser.ParserSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad (liftM)
import Text.Parsec (ParseError, parse)

-- Import the parser module from your library
import Parser

-- | All tests - just 5 unit tests and 5 property tests
tests =
    [ testGroup "Unit tests" unitTests
    , testGroup "Property-based tests" propertyTests
    ]

-- | Helper to run a parser on input and check against expected AST
parseTest :: (Show a, Eq a) => (String -> Either ParseError a) -> String -> a -> Assertion
parseTest parser input expected =
    case parser input of
        Left err -> assertFailure $ "Parse error: " ++ show err
        Right result -> assertEqual ("Parsing: " ++ input) expected result

-- | Unit Tests (5 total)

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
test_ifStatement =
    parseTest
        parseProgram
        "if x % 2 { print \"odd\"; } else { print \"even\"; };"
        ( Program
            [ If
                (Mod (Var "x") (IntLit 2))
                [PrintExpr (StringLit "odd")]
                [PrintExpr (StringLit "even")]
            ]
        )

-- Test foreach statement
test_foreachStatement :: Assertion
test_foreachStatement =
    parseTest
        parseProgram
        "foreach 1 to 5 as i { print i; };"
        (Program [ForEach 1 5 "i" [PrintExpr (Var "i")]])

-- | QuickCheck Properties (5 total)

-- Generator for valid identifiers (variable names)
genIdentifier :: Gen String
genIdentifier = do
    first <- elements $ ['a' .. 'z'] 
    rest <- listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']
    return (first : take 10 rest) -- Limit identifier length for readability

-- Generator for simple string literals
genStringLit :: Gen String
genStringLit =
    resize 20 $
        listOf $
            elements $
                ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' ', '!', '?', '.', ',']

-- Generator for small integers
genSmallInt :: Gen Int
genSmallInt = choose (-100, 100)
genSmallPositiveInt :: Gen Int
genSmallPositiveInt = choose (1, 200)


-- Generator for simple expressions
genExpr :: Int -> Gen Expr
genExpr 0 =
    oneof
        [ IntLit <$> genSmallInt
        , StringLit <$> genStringLit
        , Var <$> genIdentifier
        ]
genExpr n
    | n > 0 =
        let subexpr = genExpr (n - 1)
         in frequency
                [ (3, IntLit <$> genSmallInt)
                , (3, StringLit <$> genStringLit)
                , (3, Var <$> genIdentifier)
                , (2, Add <$> subexpr <*> subexpr)
                , (1, Mod <$> subexpr <*> subexpr)
                , (2, Concat <$> subexpr <*> subexpr)
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
            compact = "let " ++ varName ++ "= " ++ show value ++ ";"
            spaced = "let  " ++ varName ++ "  = " ++ show value ++ "  ;"
         in
            parseProgram compact === parseProgram spaced

-- Test groups
unitTests =
    [ testCase "Parse integer literal" test_intLit
    , testCase "Parse string literal" test_strLit
    , testCase "Parse think statement" test_think
    , testCase "Parse if statement" test_ifStatement
    , testCase "Parse foreach statement" test_foreachStatement
    ]

propertyTests =
    [ testProperty "Integer literals parse correctly" prop_intLit
    , testProperty "String literals parse correctly" prop_strLit
    , testProperty "Variable binding can be parsed" prop_varBinding
    , testProperty "Random expressions can be parsed" prop_randomExpr
    , testProperty "Whitespace is insignificant" prop_whitespace
    ]
