# MiniGC: A Minimal Language for Exploring Garbage Collection Techniques
## Project Overview

MiniGC is a minimal programming language designed to demonstrate different garbage collection strategies. The language includes only essential features needed to create interesting memory allocation patterns.

### Key Features
- **Simple Type System**: Primitives and heap-allocated objects
- **Explicit Allocation**: Clear memory allocation points
- **Pluggable GC**: Multiple garbage collection implementations
- **Visualization**: Memory state visualization

## Language Syntax

### AST Definition
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

### Example Program: Object Graph Creation

```
-- Create a linked list node
def createNode(value, next) = 
  new ["value", "next"] [value, next]

-- Main function with circular references
def main() = {
  let a = new ["data", "ref"] [42, null];
  let b = new ["data", "ref"] [84, null];
  
  -- Create circular reference
  a.ref = b;
  b.ref = a;
  
  -- Create a linked list
  let list = createNode(1, 
              createNode(2, 
                createNode(3, null)));
  
  -- Force some garbage
  let unusedArray = newArray 1000 0;
  
  list
}
```

## Implementation

The implementation includes:

1. **Parser**: Converts text to AST
2. **Memory Manager**: Handles allocation
3. **Garbage Collectors**: 
 One of:
   - Mark-and-Sweep
   - Reference Counting
   - Copying Collector
4. **Runtime**: Executes programs
5. **Visualizer**: Displays memory layout and GC process

### Memory Model
- **Heap**: Region for dynamically allocated objects
- **Stack**: For local variables
- **Root Set**: Stack variables and global references
