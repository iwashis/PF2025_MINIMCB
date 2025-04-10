# MathLang: A Language for Linear Algebra and Calculus

## Project Overview
MathLang is a domain-specific language designed to model linear algebra and calculus expressions with mathematical precision. The language provides an intuitive syntax that closely mirrors mathematical notation while maintaining rigorous semantics.

## Key Goals
1. **Parser Implementation**: Convert mathematical notation into an AST
2. **Expression Evaluator & Symbolic Engine**: Perform calculations and symbolic manipulations
3. **Test Suite**: Ensure correctness through unit and property-based testing

## Simplified Syntax Definition

```haskell
-- A Program is a collection of definitions and an expression to evaluate
data Program = Program [Definition] Expr
  deriving (Show, Eq)

-- Definitions for functions and variables
data Definition 
  = FuncDef String [String] Expr
  | VarDef String Expr
  deriving (Show, Eq)

-- Expressions
data Expr
  = Var String                     -- Variable reference
  | ScalarLit Double               -- Scalar literal
  | VectorLit [Expr]               -- Vector literal
  | MatrixLit [[Expr]]             -- Matrix literal
  | Add Expr Expr                  -- Addition
  | Sub Expr Expr                  -- Subtraction
  | Mul Expr Expr                  -- Multiplication (scalar, vector, or matrix)
  | Div Expr Expr                  -- Division
  | Dot Expr Expr                  -- Dot product
  | Transpose Expr                 -- Matrix transpose
  | Derivative Expr Expr           -- Partial derivative (with respect to)
  | Apply Expr [Expr]              -- Function application
  | Lambda [String] Expr           -- Anonymous function
  | Let String Expr Expr           -- Local binding
  deriving (Show, Eq)
```

## Example Program
```
-- Define a matrix multiplication function
def matmul(A, B) =
  A * B

-- Define a simple loss function
def loss(X, y, theta) = 
  let predictions = X * theta in
  let errors = predictions - y in
  (1/2) * dot(errors, errors)

-- Define gradient of loss function 
def gradient(X, y, theta) =
  derivative(loss(X, y, theta), theta)

-- Main program
let X = [[1, 2], [1, 3], [1, 4]] in
let y = [2, 3.5, 4.8] in
let initialTheta = [0, 0] in
  
-- Calculate gradient at initial point
gradient(X, y, initialTheta)
```

## Implementation Components

### 1. Parser
- Process mathematical notation into AST
- Handle variable and function definitions
- Support vector and matrix literals
- Parse mathematical operators with proper precedence
- Generate meaningful error messages for syntax issues
- Support comments and basic error recovery

### 2. Expression Evaluator & Symbolic Engine
- Implement numeric evaluation for basic operations
- Handle vector and matrix operations (addition, multiplication, dot product)
- Perform symbolic differentiation
- Apply algebraic simplification rules
- Implement dimensional analysis for type checking
- Support function application and composition
- Provide clear error messages for dimension mismatches

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Basic arithmetic operations
  - Vector and matrix operations
  - Derivative calculations
  - Function application
  - Expression simplification
  - Variable binding and scoping
  
- **Property-Based Testing**:
  - Generate random valid expressions
  - Verify mathematical properties (associativity, distributivity)
  - Test dimensional consistency
  - Check derivative rules (product rule, chain rule)
  - Ensure matrix operation correctness
  - Test symbolic simplification properties
