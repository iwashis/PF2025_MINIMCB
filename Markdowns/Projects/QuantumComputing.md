# Quantum Computing Language

## Project Overview

This project implements a domain-specific language for expressing quantum computing algorithms. The language provides abstractions for common quantum operations such as qubit initialization, gate applications, measurements, and classical control flow based on measurement results.

### Key Features

- **Quantum Gate Operations**: Express standard quantum gates (Hadamard, Pauli-X/Y/Z, CNOT, etc.)
- **Measurement Operations**: Measure qubits and store results in variables
- **Classical Control**: Conditional execution based on measurement results
- **Circuit Composition**: Build complex quantum algorithms from simple operations

### Applications

- Quantum algorithm development and testing
- Educational tool for quantum computing concepts
- Simulation of quantum protocols (teleportation, superdense coding)
- Quantum circuit optimization research

## Language Syntax

The language uses a simple, imperative style with operations that reflect quantum computing concepts directly.

### Haskell AST Definition

```haskell
-- The Program is a sequence of statements
data Program = Program [Statement]
  deriving (Show, Eq)

-- Statements represent quantum operations
data Statement
  = InitQubit Int                     -- Initialize n qubits
  | Hadamard Int                      -- Apply Hadamard gate to qubit n
  | PauliX Int                        -- Apply NOT (X) gate to qubit n 
  | PauliY Int                        -- Apply Y gate to qubit n
  | PauliZ Int                        -- Apply Z gate to qubit n
  | CNOT Int Int                      -- Controlled-NOT with control and target qubits
  | Phase Double Int                  -- Phase rotation by angle
  | PhaseRotation Int Double          -- Phase rotation by fraction of π
  | Measure Int String                -- Measure qubit n, store in variable
  | If String [Statement]             -- Conditional execution based on measurement
  | Repeat Int [Statement]            -- Repeat statements n times
  | Reset Int                         -- Reset qubit to |0⟩ state
  | Barrier [Int]                     -- Prevent optimization across a set of qubits
  | Print String                      -- Output a variable or message
  deriving (Show, Eq)
```

### Example Program: Quantum Teleportation

Here's a text representation of a quantum teleportation algorithm before parsing:

```
// Quantum Teleportation Protocol
INIT 3              // Initialize 3 qubits
HADAMARD 0          // Create superposition on qubit 0
PAULIX 0            // Apply NOT gate to qubit 0
ROTATE 0 PI/4       // Apply phase rotation

// Create Bell pair
HADAMARD 1
CNOT 1 2            // Entangle qubits 1 and 2

// Bell measurement
CNOT 0 1
HADAMARD 0
MEASURE 0 -> m1
MEASURE 1 -> m2

// Corrections based on measurement
IF m1 THEN PAULIZ 2
IF m2 THEN PAULIX 2

// Verify teleportation
MEASURE 2 -> result
PRINT result
```

## Implementation Components

The implementation consists of several key components:

1. **Parser**: Converts the text representation to an abstract syntax tree
2. **Validator**: Performs static analysis to catch errors before execution
3. **Simulator**: Executes the quantum operations using linear algebra
4. **Optimizer**: Simplifies quantum circuits before execution
5. **Visualizer**: Renders quantum circuits as diagrams

### Execution Model

The language interpreter simulates quantum operations by maintaining a state vector or density matrix representation of the quantum system. Gate operations are implemented as matrix multiplications, and measurements cause probabilistic collapse of the quantum state.
