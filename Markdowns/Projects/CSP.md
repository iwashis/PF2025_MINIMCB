# Constraint Satisfaction Programming Language

## Project Overview

This project implements a domain-specific language for expressing and solving constraint satisfaction problems (CSPs). The language provides a declarative way to define variables with their domains and constraints between them, allowing the solver to find values that satisfy all constraints simultaneously.

### Key Features

- **Declarative Constraint Specification**: Define what conditions must be satisfied, not how to find a solution
- **Rich Constraint Library**: Common constraints like equality, inequality, all-different
- **Flexible Domain Definitions**: Support for integer ranges, discrete sets, and custom domains
- **Global Constraints**: Efficient handling of constraints that affect many variables
- **Optimization Support**: Find not just any solution, but the best one according to an objective function

### Applications

- Scheduling (timetables, resource allocation)
- Planning and configuration problems
- Puzzle solving (Sudoku, crosswords)
- Routing and logistics optimization
- Design and layout problems

## Language Syntax

The language uses a declarative style where variables and constraints are specified separately, with domain definitions for each variable.

### Haskell AST Definition

```haskell
-- A Program consists of variables, constraints, and optional objective
data Program = Program [Variable] [Constraint] (Maybe Objective)
  deriving (Show, Eq)

-- Variables have names and domains
data Variable = Variable String Domain
  deriving (Show, Eq)

-- Different types of domains for variables
data Domain
  = IntRange Int Int                -- Integer range (min, max)
  | DiscreteSet [Value]             -- Set of discrete values
  | BoolDomain                      -- Boolean domain (true/false)
  | CustomDomain String             -- Named custom domain
  deriving (Show, Eq)

-- Value types for domains and expressions
data Value
  = IntVal Int
  | StringVal String
  | BoolVal Bool
  | ListVal [Value]
  deriving (Show, Eq)

-- Constraint types
data Constraint
  = Equal Variable Variable                 -- Variables must be equal
  | NotEqual Variable Variable              -- Variables must be different
  | LessThan Variable Variable              -- One variable less than another
  | GreaterThan Variable Variable           -- One variable greater than another
  | AllDifferent [Variable]                 -- All variables must have different values
  | SumEquals [Variable] Int                -- Sum must equal a value
  | Relation [Variable] String              -- Custom relation (table constraint)
  | ConditionalConstraint Expr Constraint   -- Constraint applies if expression is true
  deriving (Show, Eq)

-- Expression for conditions and objectives
data Expr
  = VarRef String                  -- Reference to a variable
  | Literal Value                  -- Literal value
  | Plus Expr Expr                 -- Addition
  | Minus Expr Expr                -- Subtraction
  | Times Expr Expr                -- Multiplication
  | Div Expr Expr                  -- Division
  | Mod Expr Expr                  -- Modulo
  | Eq Expr Expr                   -- Equality comparison
  | Neq Expr Expr                  -- Inequality comparison
  | Lt Expr Expr                   -- Less than comparison
  | Gt Expr Expr                   -- Greater than comparison
  | And Expr Expr                  -- Logical AND
  | Or Expr Expr                   -- Logical OR
  | Not Expr                       -- Logical NOT
  deriving (Show, Eq)

-- Objective function for optimization
data Objective
  = Minimize Expr                  -- Find solution with minimum value
  | Maximize Expr                  -- Find solution with maximum value
  deriving (Show, Eq)
```

### Example Program: University Course Scheduling

Here's a text representation of a course scheduling problem before parsing:

```
// University Course Scheduling

// Define course time slots
VAR CS101: ["Mon9", "Mon14", "Tue11", "Thu9"];
VAR CS202: ["Mon11", "Tue9", "Tue14", "Wed11"];
VAR CS303: ["Mon14", "Tue14", "Wed9", "Thu14"];
VAR CS404: ["Mon9", "Wed14", "Thu11", "Fri9"];
VAR CS505: ["Tue11", "Wed9", "Thu14", "Fri11"];

// Define professors and their courses
VAR ProfSmith: ["CS101", "CS303"];
VAR ProfJones: ["CS202", "CS505"];
VAR ProfBrown: ["CS404"];

// Define rooms with capacities
VAR RoomA101: 30..100;
VAR RoomB205: 20..50;
VAR RoomC310: 10..30;

// Course enrollment numbers
VAR CS101_size: 45;
VAR CS202_size: 38;
VAR CS303_size: 25;
VAR CS404_size: 15;
VAR CS505_size: 12;

// Time conflict constraints
CONSTRAINT time(CS101) != time(CS202);
CONSTRAINT time(CS101) != time(CS303);
CONSTRAINT time(CS202) != time(CS505);
CONSTRAINT time(CS303) != time(CS404);

// Professor time conflict constraints
CONSTRAINT time(ProfSmith.CS101) != time(ProfSmith.CS303);
CONSTRAINT time(ProfJones.CS202) != time(ProfJones.CS505);

// Room capacity constraints
CONSTRAINT room_capacity(CS101) >= CS101_size;
CONSTRAINT room_capacity(CS202) >= CS202_size;
CONSTRAINT room_capacity(CS303) >= CS303_size;
CONSTRAINT room_capacity(CS404) >= CS404_size;
CONSTRAINT room_capacity(CS505) >= CS505_size;

// Minimize the number of different days used
MINIMIZE count_distinct_days(CS101, CS202, CS303, CS404, CS505);

// Find solution
SOLVE;
PRINT_SCHEDULE;
```

## Implementation Components

The implementation consists of several key components:

1. **Parser**: Converts the textual representation to variables and constraints
2. **Constraint Store**: Manages variables, domains, and constraints
3. **Constraint Propagator**: Applies constraint propagation to reduce domains
4. **Search Engine**: Explores the solution space using backtracking and heuristics
5. **Solution Validator**: Verifies that all constraints are satisfied
6. **Visualizer**: Renders the solution in a user-friendly format

### Execution Model

The constraint solver uses a combination of:
1. **Constraint Propagation**: Reduce variable domains based on constraints
2. **Search**: Systematically try values for variables
3. **Backtracking**: Undo assignments that lead to contradictions
4. **Heuristics**: Guide search to likely successful assignments first
