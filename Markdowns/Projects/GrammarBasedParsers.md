# Grammar-Based Parsing Language

## Project Overview
This project implements a domain-specific language for defining formal grammars and generating parsers from them. The language allows users to specify syntax rules in a readable format, associate semantic actions with parsing events, and generate parsers that convert text into structured representations.

## Key Goals
1. **Parser Implementation**: Convert grammar specifications into an AST
2. **Grammar Analyzer & Parser Generator**: Analyze grammar structure and generate parsing code
3. **Test Suite**: Ensure correctness through unit and property-based testing

## Simplified Syntax Definition

```haskell
-- A Program is a collection of grammar rules and lexer rules
data Program = Program [Rule] [LexerRule]
  deriving (Show, Eq)

-- A grammar rule defines a nonterminal and its productions
data Rule = Rule Nonterminal [Production] (Maybe Action)
  deriving (Show, Eq)

-- A lexer rule defines a token pattern
data LexerRule = LexerRule String String (Maybe Action)
  deriving (Show, Eq)

-- A nonterminal symbol in the grammar
data Nonterminal = Nonterminal String
  deriving (Show, Eq)

-- Core production types
data Production
  = Sequence [Symbol]              -- Sequence of symbols
  | Choice [Production]            -- Alternative productions
  | Optional Production            -- Optional production (0 or 1)
  | Repeat Production              -- Zero or more repetitions
  | RepeatOneOrMore Production     -- One or more repetitions
  | Action Production String       -- Semantic action
  deriving (Show, Eq)

-- Symbols that can appear in a production
data Symbol
  = Terminal String                -- Terminal symbol
  | NonterminalSymbol Nonterminal  -- Nonterminal symbol
  | Epsilon                        -- Empty string
  deriving (Show, Eq)

-- Action associated with a rule or production
data Action = Action String
  deriving (Show, Eq)
```

## Example Grammar
```
// Simple Arithmetic Expression Grammar

// Main expression structure
Expr ::= Term (("+" | "-") Term)*
       @ BuildExprNode;

// Term in an expression
Term ::= Factor (("*" | "/") Factor)*
       @ BuildTermNode;

// Factor can be a number, variable, or parenthesized expression
Factor ::= Number | Variable | "(" Expr ")"
         @ BuildFactorNode;

// Generate lexer rules
@lexer
Number ::= [0-9]+ ("." [0-9]+)?;
Variable ::= [a-zA-Z][a-zA-Z0-9_]*;
Whitespace ::= [ \t\n\r]+ @Skip;
```

## Implementation Components

### 1. Parser
- Process grammar specifications into AST
- Handle rule definitions and production alternatives
- Support lexer rule definitions
- Parse semantic actions
- Generate meaningful error messages for syntax issues
- Support comments and basic error recovery

### 2. Grammar Analyzer & Parser Generator
- Analyze grammar for conflicts and ambiguities
- Generate recursive descent parser code or parser tables
- Implement lexical analyzer generation
- Support semantic action integration
- Provide FIRST and FOLLOW set calculation
- Generate straightforward parser code in Haskell

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for grammar specifications
  - Parser generation for simple grammars
  - Action integration in generated parsers
  
- **Property-Based Testing**:
  - Generate random valid grammars
  - Test parser generator correctness
  - Validate recursive descent generation
