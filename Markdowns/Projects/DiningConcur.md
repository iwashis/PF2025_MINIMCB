# Concurrent Programming Language

Taken (TB)

## Project Overview
This project implements a minimal domain-specific language for expressing concurrent programs with a focus on resource management and synchronization. The language provides abstractions for parallel processes, atomic resource acquisition, and structured concurrency, making it well-suited for solving classic concurrency problems.

## Key Goals
1. **Parser Implementation**: Convert textual concurrent program definitions into an AST
2. **Interpreter & Resource Manager**: Execute concurrent programs with proper resource handling
3. **Test Suite**: Ensure correctness through unit and property-based testing

## Simplified Syntax Definition

```haskell
-- A Program is a sequence of statements
data Program = Program [Statement]
  deriving (Show, Eq)

-- Resource type for protected objects
data Resource = Resource String
  deriving (Show, Eq)

-- Expression type for integer operations
data Expr
  = Var String                  -- Variable reference
  | StringLit String            -- String literal
  | Concat Expr Expr            -- String concatenation
  | IntLit Int                  -- Integer literal
  | Rand Expr Expr              -- Random integer in range
  | Add Expr Expr               -- Addition
  | Sub Expr Expr               -- Subtraction
  | Mod Expr Expr               -- Modulo operation
  deriving (Show, Eq)

-- Statement constructs
data Statement
  = Think Expr                             -- Think for computed time units
  | Eat Expr Resource Resource             -- Eat for computed time units using two resources
  | Print String                           -- Print to stdout
  | DeclareResource String                 -- Declare a global mutex resource 
  | Loop [Statement]                       -- Loop indefinitely
  | Spawn Expr [Statement]                 -- Spawn named process with statements
  | LockAll [Expr] [String]                -- Atomically lock resources, binding results to variables
  | UnlockAll [Resource]                   -- Atomically unlock multiple resources
  | Let String Expr                        -- Variable binding
  | ForEach Int Int String [Statement]     -- Iterate from start to end, binding index to variable
  | If Expr [Statement] [Statement]        -- Conditional execution
  deriving (Show, Eq)
```

## Example Program
```
// Dining Philosophers Problem (Simplified)

// Define number of philosophers
let numPhils = 3;

// Declare fork resources
foreach 0 to 2 as i {
  declareResource "fork" ++ i;
};

// Create philosophers
foreach 0 to 2 as i {
  // Define left and right fork indices
  let leftFork = "fork" ++ i;
  let rightFork = "fork" ++ (i + 1) % numPhils;
  
  // Spawn philosopher process
  spawn "philosopher" ++ i {
    loop {
      // Think for a while
      print "Philosopher " ++ i ++ " thinking";
      think (rand 10 50);
      
      // Acquire both forks atomically
      lockAll [leftFork, rightFork] ["left", "right"];
      
      // Eat using the acquired forks
      print "Philosopher " ++ i ++ " eating";
      eat (rand 10 50) resource left resource right;
      
      // Release forks
      unlockAll [resource left, resource right];
    };
  };
};
```

## Implementation Components

### 1. Parser
- Process concurrent program definitions into an AST
- Handle expressions and statements
- Support string interpolation for identifiers
- Generate meaningful error messages for syntax issues
- Support comments and basic error recovery

### 2. Interpreter & Resource Manager
- Implement STM-based concurrent execution engine
- Manage resource allocation and contention
- Handle atomic resource acquisition operations
- Implement structured process spawning and management
- Provide logging of concurrent activities
- Track resource acquisition to detect potential deadlocks
- Simulate timing of think/eat operations

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Expression evaluation
  - Resource locking and unlocking operations
  - Process spawning and execution
  - Variable binding and scope handling
  - Individual concurrency primitives
  
- **Property-Based Testing**:
  - Generate random valid programs
  - Verify absence of deadlocks in well-formed programs
  - Test scheduler behavior under various concurrency patterns
