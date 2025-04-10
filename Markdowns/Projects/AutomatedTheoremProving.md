# Automated Theorem Proving Language

## Project Overview
This project implements a simplified domain-specific language for expressing basic mathematical proofs in a machine-verifiable format. The language provides core constructs for defining axioms, stating theorems, and building step-by-step proofs.

## Key Goals
1. **Parser Implementation**: Convert textual proof representations into structured AST
2. **Proof Evaluator & Rule Engine**: Validate proof correctness and apply inference rules
3. **Test Suite**: Comprehensive testing of language features and proof verification

## Simplified Syntax Definition

```haskell
-- Program representation
data Program = Program [Statement]
  deriving (Show, Eq)

-- Logical formulas (propositional logic only)
data Formula
  = Var String                     -- Variable
  | Const Bool                     -- Constant true/false
  | And Formula Formula            -- Logical AND
  | Or Formula Formula             -- Logical OR
  | Not Formula                    -- Logical NOT
  | Implies Formula Formula        -- Implication
  deriving (Show, Eq)

-- Statements in the proof language
data Statement
  = Axiom String Formula           -- Define an axiom
  | Theorem String Formula         -- State a theorem
  | Given Formula                  -- Assumption for a proof
  | Assert Formula                 -- Claim without justification
  | Apply String [Formula]         -- Apply a rule or theorem
  | ByContradiction [Statement]    -- Proof by contradiction
  | Qed                            -- End of proof
  deriving (Show, Eq)
```

## Example Proof
```
// Simple Proof Example
AXIOM contrapositive: (P → Q) ↔ (¬Q → ¬P);
THEOREM simple_implication: P → Q → (¬Q → ¬P);
PROOF {
  GIVEN P → Q;
  APPLY contrapositive TO (P → Q) GET (¬Q → ¬P);
  QED;
}
```

## Implementation Components

### 1. Parser
- Process proof language into structured AST
- Handle logical operators and proof statements
- Support Unicode mathematical symbols
- Report syntax errors with meaningful messages
- Support comments and basic error recovery

### 2. Proof Evaluator & Rule Engine
- Track proof context (axioms, given statements)
- Implement basic inference rules of propositional logic
- Validate each proof step for logical correctness
- Apply substitution and pattern matching for rule application
- Generate error messages for invalid proof steps

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Rule application validation
  - Individual proof step verification
- **Property-Based Testing**:
  - Generate random formulas and verify properties

