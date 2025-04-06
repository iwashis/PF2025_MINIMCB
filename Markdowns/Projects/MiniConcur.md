# MiniComm: A Minimal Concurrent Language

## Project Overview

MiniComm is a minimal language for expressing concurrent programs with a focus on message passing. The language provides just the essential primitives needed for concurrent computation while maintaining mathematical clarity.

### Key Features

- **Processes**: Simple concurrent execution units
- **Channels**: Typed communication channels between processes
- **Message Passing**: Synchronous communication primitives
- **Basic Arithmetic**: Essential integer operations

## Language Syntax

### AST Definition

```haskell
-- A Program is a collection of process definitions and a main process
data Program = Program [ProcessDef] Process
  deriving (Show, Eq)

-- A process definition has a name and a body
data ProcessDef = ProcessDef String [String] Process
  deriving (Show, Eq)

-- Channels for communication
data Channel = Channel String
  deriving (Show, Eq)

-- Expressions
data Expr
  = Var String        -- Variable reference
  | IntLit Int        -- Integer literal
  | Add Expr Expr     -- Addition
  | Sub Expr Expr     -- Subtraction
  | Mul Expr Expr     -- Multiplication
  deriving (Show, Eq)

-- Process constructs
data Process
  = Send Channel Expr              -- Send value on channel
  | Receive Channel String         -- Receive from channel into variable
  | New String Process             -- Create new channel
  | Par Process Process            -- Parallel composition
  | Let String Expr Process        -- Local binding
  | Call String [Expr]             -- Process invocation
  | If Expr Process Process        -- Conditional
  | Print Expr                     -- Print expression value
  | Zero                           -- Inaction (null process)
  deriving (Show, Eq)
```

### Example Program: Producer-Consumer

```
-- Define producer process
def Producer(out, count, value) =
  if count < 10 then
    print(value).
    out!value.
    Producer(out, count+1, value+1)
  else
    print("Producer finished").
    0

-- Define consumer process
def Consumer(in) =
  in?value.
  print("Received: " + value).
  Consumer(in)

-- Main program
new channel in
( Producer(channel, 0, 1) | Consumer(channel) )
```

## Implementation

The implementation consists of:

1. **Lexer & Parser**: Converts text to AST
2. **Type Checker**: Basic type checking for expressions and channels
3. **Interpreter**: Executes processes with channel synchronization
4. **Scheduler**: Basic round-robin scheduler for processes
5. **Channel Manager**: Handles synchronization between processes

### Execution Model

- Processes execute concurrently
- Channel operations are synchronous (sender and receiver must rendezvous)
- Deterministic scheduling for reproducible execution

### Implementation Considerations

1. **Process Representation**:
   - Use lightweight threads or cooperative scheduling
   - Maintain process state and environment
   - Track channel dependencies for each process

2. **Channel Implementation**:
   - Implement as rendezvous points between processes
   - Use queues to track blocked senders and receivers
   - Ensure fair selection when multiple processes are waiting

3. **Deadlock Detection**:
   - Track dependency graph between processes
   - Detect cycles in the waiting graph
   - Provide diagnostic information when deadlocks occur

4. **Concurrency Model**:
   - Consider using Communicating Sequential Processes (CSP) semantics
   - Implement using continuation-passing style for efficiency
   - Ensure isolation between process environments

5. **Printing and I/O**:
   - Handle printing asynchronously to avoid blocking execution
   - Provide ordered output based on process execution
   - Include process IDs in output for debugging

6. **Testing Strategies**:
   - Deterministic scheduling for reproducible tests
   - Controlled interleaving of process execution
   - Invariant checking across multiple runs
