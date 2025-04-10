# MiniGC: A Minimal Language for Exploring Garbage Collection

## Project Overview
MiniGC is a minimal programming language designed to demonstrate different garbage collection strategies. The language includes only essential features needed to create interesting memory allocation patterns while providing a framework to implement and compare various garbage collection techniques.

## Key Goals
1. **Parser Implementation**: Convert MiniGC programs into an AST
2. **Interpreter & Memory Manager**: Execute programs with memory allocation tracking
3. **Test Suite**: Ensure correctness through unit and property-based testing

## Simplified Syntax Definition

```haskell
-- Program with function definitions and main expression
data Program = Program [FuncDef] Expr
  deriving (Show, Eq)

-- Function definition
data FuncDef = FuncDef String [String] Expr
  deriving (Show, Eq)

-- Types
data Type
  = TInt                -- Integer
  | TBool               -- Boolean
  | TObject [String]    -- Object with field names (all fields are references)
  | TArray              -- Array of references
  deriving (Show, Eq)

-- Expressions
data Expr
  = Var String                    -- Variable
  | IntLit Int                    -- Integer literal
  | BoolLit Bool                  -- Boolean literal
  | BinOp BinOp Expr Expr         -- Binary operation
  | If Expr Expr Expr             -- Conditional
  | Let String Expr Expr          -- Local binding
  | Call String [Expr]            -- Function call
  | New [String] [Expr]           -- Object allocation with field names and values
  | NewArray Expr Expr            -- Array allocation with size and init value
  | FieldAccess Expr String       -- Field access
  | FieldAssign Expr String Expr  -- Field assignment
  | ArrayAccess Expr Expr         -- Array access
  | ArrayAssign Expr Expr Expr    -- Array assignment
  | Seq Expr Expr                 -- Expression sequence
  | Null                          -- Null reference
  deriving (Show, Eq)

-- Binary operators
data BinOp = Add | Sub | Eq
  deriving (Show, Eq)
```

## Example Program: Memory Leak Detection

```
-- Create a circular reference structure
def createCycle() = {
  let a = new ["data", "ref"] [42, null];
  let b = new ["data", "ref"] [84, null];
  
  -- Create circular reference
  a.ref = b;
  b.ref = a;
  
  -- Return null, making the cycle unreachable
  null
}

-- Main function that allocates memory and creates garbage
def main() = {
  -- Create a linked list
  let list = createNode(1, 
              createNode(2, 
                createNode(3, null)));
  
  -- Create unreachable memory (garbage)
  createCycle();
  
  -- Create and immediately abandon a large array
  let temp = newArray 100 0;
  
  -- Return the list as the result
  list
}

-- Helper to create a linked list node
def createNode(value, next) = 
  new ["value", "next"] [value, next]
```

This example demonstrates:
1. A linked list structure with references between objects
2. Creation of circular references that require proper GC
3. Unreachable objects that should be collected
4. Different allocation patterns (objects and arrays)

## Implementation Components

### 1. Parser
- Process MiniGC programs into AST
- Handle function definitions and expressions
- Support object and array allocation syntax
- Generate meaningful error messages for syntax issues
- Support comments and basic error recovery

### 2. Interpreter & Memory Manager
- Implement an environment-based interpreter
- Track object allocations on a simulated heap
- Maintain object references and the root set
- Implement a basic mark-and-sweep garbage collector
- Support memory usage statistics and reporting
- Detect and handle circular references properly
- Visualize memory state with simple text-based output

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Memory allocation operations
  - Object field access and modification
  - Garbage collection correctness
  
- **Property-Based Testing**:
  - Generate random valid MiniGC programs
  - Verify memory safety properties
  - Test garbage collector correctness
