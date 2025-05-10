module Parser where


import Text.Parsec
import Data.List (intercalate)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)

-- Abstract Syntax Tree definitions as provided in the project description
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
-- Lexer definition

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

-- Parser utilities from the lexer
identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

semi :: Parser String
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

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

