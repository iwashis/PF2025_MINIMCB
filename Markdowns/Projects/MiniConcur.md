# MiniComm: A Minimal Concurrent Language

## Project Overview
MiniComm is a minimal language for expressing concurrent programs with a focus on message passing. The language provides just the essential primitives needed for concurrent computation while maintaining mathematical clarity and simplicity.

## Key Goals
1. **Parser Implementation**: Convert MiniComm programs into an AST
2. **Interpreter & Channel Manager**: Execute concurrent processes with proper channel synchronization
3. **Test Suite**: Ensure correctness through unit and property-based testing

## Simplified Syntax Definition

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

## Example Program
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

## Implementation Components

### 1. Parser
- Process MiniComm program text into AST
- Handle process definitions and expressions
- Support channel declaration and operations
- Generate meaningful error messages for syntax issues
- Support comments and basic error recovery
- Parse process composition and invocation

### 2. Interpreter & Channel Manager
- Implement concurrent execution engine for processes
- Manage channel creation and synchronization
- Handle rendezvous-style message passing
- Implement deterministic process scheduling
- Support process recursion through calls
- Manage variable bindings and environments
- Provide execution tracing and debugging info

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Expression evaluation
  - Channel operations (send/receive)
  - Process creation and invocation
  - Process termination conditions
  
- **Property-Based Testing**:
  - Generate random valid programs
  - Verify message passing correctness
  - Test synchronization properties
  - Test recursive process behavior
