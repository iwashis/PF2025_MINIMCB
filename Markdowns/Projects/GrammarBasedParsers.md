# Grammar-Based Parsing Language

## Project Overview

This project implements a domain-specific language for defining formal grammars and generating parsers from them. The language allows users to specify syntax rules, associating semantic actions with parsing events, and generate efficient parsers that convert text into structured representations.

### Key Features

- **Grammar Rule Definition**: Express language syntax in a readable format
- **Production Alternatives**: Support for choice, sequence, repetition
- **Semantic Actions**: Associate code with successful parsing events
- **Error Handling**: Customizable error reporting and recovery
- **Lexical Analysis**: Integrated token definition and recognition

### Applications

- Compiler and interpreter development
- Domain-specific language implementation
- Data format parsing (configuration files, markup)
- Natural language processing components
- Protocol message parsing

## Language Syntax

The language uses a notation similar to Extended Backus-Naur Form (EBNF) with extensions for semantic actions and parser directives.

### Haskell AST Definition

```haskell
-- A Program is a collection of grammar rules
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

-- Different types of productions in a grammar rule
data Production
  = Sequence [Symbol]              -- Sequence of symbols
  | Choice [Production]            -- Alternative productions
  | Optional Production            -- Optional production (0 or 1)
  | Repeat Production              -- Zero or more repetitions
  | RepeatOneOrMore Production     -- One or more repetitions
  | Group Production               -- Grouping for clarity
  | Lookahead Production Bool      -- Positive or negative lookahead
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

### Example Program: SQL Query Parser

Here's a text representation of an SQL parser grammar before parsing:

```
// SQL Query Parser Grammar

// Main query structure
Query ::= SelectClause FromClause [WhereClause] [GroupByClause]
          [HavingClause] [OrderByClause]
        @ BuildQueryObject;

// Select clause
SelectClause ::= "SELECT" ["DISTINCT" | "ALL"] ColumnList
              @ BuildSelectClause;

// From clause
FromClause ::= "FROM" TableReferences
             @ BuildFromClause;

// Where clause
WhereClause ::= "WHERE" Expression
              @ BuildWhereClause;

// Column list can be * or a list of column items
ColumnList ::= "*" | ColumnItem ("," ColumnItem)*
             @ BuildColumnList;

// Column item with optional alias
ColumnItem ::= Expression ["AS" Identifier]
             @ BuildColumnItem;

// Expression can be various types
Expression ::= BinaryExpression | FunctionCall | Column | Literal
             @ EvaluateExpression;

// Binary expression with operator
BinaryExpression ::= Expression Operator Expression
                   @ BuildBinaryExpr;

// Operators
Operator ::= "=" | "<" | ">" | "<=" | ">=" | "!=" | "LIKE" | "IN"
           @ BuildOperator;

// Function call
FunctionCall ::= Identifier "(" [Expression ("," Expression)*] ")"
               @ BuildFunctionCall;

// Column reference
Column ::= [TableName "."] Identifier
         @ BuildColumnRef;

// Generate lexer rules
@lexer
Identifier ::= [a-zA-Z][a-zA-Z0-9_]*;
StringLiteral ::= "'" [^']* "'";
NumberLiteral ::= [0-9]+ ("." [0-9]+)?;
```

## Implementation Components

The implementation consists of several key components:

1. **Parser Generator**: Converts grammar specification to parser code
   - Implements LL, LR, LALR, or PEG parsing algorithms
   - Generates lexical analyzer for tokens

2. **Parse Tree Builder**: Constructs syntax tree
   - Creates nodes for grammar rules
   - Associates semantic actions with nodes

3. **Semantic Analyzer**: Processes parse tree
   - Executes semantic actions
   - Builds abstract syntax tree (AST)

4. **Symbol Table Manager**: Handles identifiers
   - Tracks scope and bindings
   - Resolves references

5. **Error Handler**: Reports syntax errors
   - Provides meaningful error messages
   - Implements error recovery strategies

6. **Code Generator**: Produces parser implementation
   - Generates code in target language (e.g., Haskell)
   - Optimizes for performance

### Execution Model

The grammar-based parsing system works in several phases:
1. Parse the grammar definition
2. Analyze the grammar for conflicts and ambiguities
3. Generate parser tables or recursive descent functions
4. Generate lexical analyzer code
5. Compile the resulting parser into executable code

During parsing of target input:
1. The lexer tokenizes the input
2. The parser builds a parse tree according to grammar rules
3. Semantic actions are executed at appropriate points
4. The resulting data structure (often an AST) is returned
