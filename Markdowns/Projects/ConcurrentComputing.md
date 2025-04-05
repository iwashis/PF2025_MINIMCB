# Concurrent Programming Language

## Project Overview

This project implements a minimal domain-specific language for expressing concurrent programs with a focus on resource management and synchronization. The language provides abstractions for parallel processes, atomic resource acquisition, and structured concurrency, making it particularly well-suited for solving classic concurrency problems like the dining philosophers problem.

### Key Features

- **Lightweight Process Model**: Spawn concurrent processes with minimal overhead
- **Atomic Resource Acquisition**: Lock multiple resources in one atomic operation
- **Structured Concurrency**: Clear process hierarchies and lifetimes
- **Minimal Core Concepts**: Focus on essential concurrency primitives
- **Expression-Based Resource Handling**: Resources are first-class values

### Applications

- Educational tool for concurrent programming concepts
- Prototype concurrent algorithms
- Model and detect concurrency issues like deadlock
- Implement concurrent data structures
- Simulation of resource contention scenarios

## Language Syntax

The language uses a minimal imperative style with specific constructs for concurrency and synchronization.

### Haskell AST Definition

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
  | IntLit Int                  -- Integer literal
  | Add Expr Expr               -- Addition
  | Sub Expr Expr               -- Subtraction
  | Mod Expr Expr               -- Modulo operation
  | Eq Expr Expr                -- Equality comparison
  | Lt Expr Expr                -- Less than
  | Gt Expr Expr                -- Greater than
  deriving (Show, Eq)

-- Statement constructs
data Statement
  = Think Expr                             -- Think for computed time units
  | Eat Expr Resource Resource             -- Eat for computed time units using two resources
  | Loop [Statement]                       -- Loop indefinitely
  | Spawn Expr [Statement]                 -- Spawn n processes with statements
  | LockAll [Expr] [String]                -- Atomically lock resources, binding results to variables
  | UnlockAll [Resource]                   -- Atomically unlock multiple resources
  | Let String Expr                        -- Variable binding
  | ForEach Int Int String [Statement]     -- Iterate from start to end, binding index to variable
  | If Expr [Statement] [Statement]        -- Conditional execution
  deriving (Show, Eq)
```

### Example Program: Dining Philosophers

Here's a text representation of the dining philosophers problem before parsing:

```
// Dining Philosophers Problem

// Define number of philosophers
let numPhils = 5;

// For each philosopher
foreach 0 to 4 as i {
  // Define left and right fork indices
  let leftFork = i;
  let rightFork = (i + 1) % numPhils;
  
  // Spawn philosopher process
  spawn 1 {
    // Repeat indefinitely
    loop {
      // Think for a while
      think 5;
      
      // Acquire both forks atomically
      lockAll [leftFork, rightFork] ["left", "right"];
      
      // Eat using the acquired forks
      eat 10 resource left resource right;
      
      // Release forks
      unlockAll [resource left, resource right];
    };
  };
};
```

## Implementation Components

The implementation consists of several key components:

1. **Parser**: Converts the textual representation to an AST
2. **Type Checker**: Validates variable usage and expression types
3. **Interpreter**: Executes the program using a concurrent runtime
4. **Scheduler**: Manages process execution and context switching
5. **Resource Manager**: Handles resource locking and contention
6. **Visualizer**: Displays process states and resource ownership

### Execution Model

The concurrent interpreter operates by:
1. Spawning lightweight threads for each process
2. Managing a shared pool of resources
3. Handling atomic resource acquisition using transactions
4. Simulating blocking operations for resource contention
5. Providing structured logging of concurrent activities

The use of Software Transactional Memory (STM) in the implementation ensures that resource acquisition is truly atomic, preventing race conditions and ensuring consistency.

### STM-Based Implementation

The interpreter uses Haskell's Software Transactional Memory (STM) to implement the concurrency features:

1. **Resources** are represented as `TMVar ()` references:
   - When the `TMVar` is full, the resource is available
   - When empty, the resource is currently owned by a process

2. **Atomic Operations** are implemented as STM transactions:
   - `LockAll` attempts to take multiple `TMVar`s in a single transaction
   - The operation blocks until all resources are available
   - Either all resources are acquired or none (preventing deadlock)

3. **Processes** run as Haskell threads with:
   - Thread-local environment for variables
   - Shared access to the resource pool
   - Simulated think/eat delays using `threadDelay`
