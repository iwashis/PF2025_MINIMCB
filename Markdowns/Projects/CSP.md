# Constraint Satisfaction Programming Language

## Project Overview
This project implements a simplified domain-specific language for expressing and solving constraint satisfaction problems (CSPs). The language provides a declarative way to define variables with their domains and constraints between them.

## Key Goals
1. **Parser Implementation**: Convert textual CSP definitions into a structured AST
2. **Constraint Store & Propagator**: Manage variables, domains, and apply constraint propagation
3. **Test Suite**: Comprehensive testing of language features and solver correctness

## Simplified Syntax Definition

```haskell
-- Program representation
data Program = Program [Variable] [Constraint] (Maybe Objective)
  deriving (Show, Eq)

-- Variables have names and domains
data Variable = Variable String Domain
  deriving (Show, Eq)

-- Simplified domain types
data Domain
  = IntRange Int Int            -- Integer range (min, max)
  | DiscreteSet [Value]         -- Set of discrete values
  | BoolDomain                  -- Boolean domain (true/false)
  deriving (Show, Eq)

-- Value types 
data Value
  = IntVal Int
  | StringVal String
  | BoolVal Bool
  deriving (Show, Eq)

-- Core constraint types
data Constraint
  = Equal Variable Variable             -- Variables must be equal
  | NotEqual Variable Variable          -- Variables must be different
  | LessThan Variable Variable          -- One variable less than another
  | GreaterThan Variable Variable       -- One variable greater than another
  | AllDifferent [Variable]             -- All variables must have different values
  deriving (Show, Eq)

-- Simplified objective function
data Objective
  = Minimize Variable           -- Find solution with minimum value
  | Maximize Variable           -- Find solution with maximum value
  deriving (Show, Eq)
```

## Example CSP Problem
```
// Map Coloring Problem
VAR WA: ["red", "green", "blue"];
VAR NT: ["red", "green", "blue"];
VAR SA: ["red", "green", "blue"];
VAR Q: ["red", "green", "blue"];
VAR NSW: ["red", "green", "blue"];
VAR V: ["red", "green", "blue"];
VAR T: ["red", "green", "blue"];

// Adjacent regions must have different colors
CONSTRAINT WA != NT;
CONSTRAINT WA != SA;
CONSTRAINT NT != SA;
CONSTRAINT NT != Q;
CONSTRAINT SA != Q;
CONSTRAINT SA != NSW;
CONSTRAINT SA != V;
CONSTRAINT Q != NSW;
CONSTRAINT NSW != V;

SOLVE;
```

## Implementation Components

### 1. Parser
- Process DSL input into variable and constraint definitions
- Handle different domain types and constraint specifications
- Report syntax errors with meaningful messages
- Support comments and basic error recovery

### 2. Constraint Store & Search Engine
- Maintain variable domains during solving
- Apply constraint propagation algorithms (AC-3)
- Implement depth-first backtracking search

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Individual constraint satisfaction checks
- **Property-Based Testing**:
  - Generate random CSPs and verify solver properties
