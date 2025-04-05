# Automated Theorem Proving Language

## Project Overview

This project implements a domain-specific language for expressing mathematical proofs in a structured, machine-verifiable format. The language provides constructs for defining axioms, stating theorems, and building step-by-step proofs using rules of inference.

### Key Features

- **Formal Proof Expression**: Rigorous mathematical proofs with explicit justification steps
- **Rule Application**: Apply standard logical rules and previously proven theorems
- **Proof Checking**: Automatic verification of proof correctness
- **Proof Strategies**: Support for common proof techniques (direct, contradiction, induction)
- **Theorem Management**: Build libraries of proven results for reuse

### Applications

- Mathematics education and teaching formal proofs
- Verification of software correctness
- Development of verified algorithms
- Formalization of mathematical theories
- Computer-assisted mathematical research

## Language Syntax

The language uses a structured approach with explicit statements representing logical formulas and proof steps.

### Haskell AST Definition

```haskell
-- A Program consists of definitions and proofs
data Program = Program [Statement]
  deriving (Show, Eq)

-- Logical formulas
data Formula
  = Var String                     -- Variable
  | Const Bool                     -- Constant true/false
  | And Formula Formula            -- Logical AND
  | Or Formula Formula             -- Logical OR
  | Not Formula                    -- Logical NOT
  | Implies Formula Formula        -- Implication
  | Equivalence Formula Formula    -- Logical equivalence
  | ForAll String Formula          -- Universal quantification
  | Exists String Formula          -- Existential quantification
  | Predicate String [Term]        -- Predicate application
  deriving (Show, Eq)

-- Terms (for predicates and functions)
data Term
  = VarTerm String                 -- Variable term
  | FuncTerm String [Term]         -- Function application
  | ConstTerm String               -- Constant term
  deriving (Show, Eq)

-- Statements in the proof language
data Statement
  = Axiom String Formula           -- Define an axiom
  | Definition String Formula      -- Define a concept
  | Theorem String Formula         -- State a theorem
  | Lemma String Formula           -- State a lemma (helper theorem)
  | Given Formula                  -- Assumption for a proof
  | Assert Formula                 -- Claim without justification (for hypotheticals)
  | Prove Formula                  -- Statement to be proven
  | Apply String [Formula]         -- Apply a rule or theorem
  | ByContradiction [Statement]    -- Proof by contradiction
  | ByCases Formula [Case]         -- Proof by cases
  | ByInduction String [Statement] -- Proof by induction
  | Qed                            -- End of proof
  deriving (Show, Eq)

-- Case in a proof by cases
data Case = Case Formula [Statement]
  deriving (Show, Eq)
```

### Example Program: DeMorgan's Law Proof

Here's a text representation of a proof of DeMorgan's law before parsing:

```
// DeMorgan's Law Proof

// Define propositional logic axioms
AXIOM excluded_middle: P ∨ ¬P;
AXIOM non_contradiction: ¬(P ∧ ¬P);
AXIOM double_negation: P ↔ ¬¬P;

// Define DeMorgan's Law theorem to prove
THEOREM demorgans_law: ¬(P ∧ Q) ↔ (¬P ∨ ¬Q);

// Start proof
PROOF {
  // First direction: ¬(P ∧ Q) → (¬P ∨ ¬Q)
  GIVEN ¬(P ∧ Q);
  CASES P {
    CASE P {
      // If P is true, then Q must be false for ¬(P ∧ Q) to hold
      ASSERT P;
      BY contradiction FROM P, ¬(P ∧ Q) GET ¬Q;
      BY disjunction_intro FROM ¬Q GET ¬P ∨ ¬Q;
    }
    CASE ¬P {
      // If P is false, then ¬P is true
      ASSERT ¬P;
      BY disjunction_intro FROM ¬P GET ¬P ∨ ¬Q;
    }
  }
  // By case analysis, we have ¬P ∨ ¬Q
  
  // Second direction: (¬P ∨ ¬Q) → ¬(P ∧ Q)
  GIVEN ¬P ∨ ¬Q;
  CASES ¬P ∨ ¬Q {
    CASE ¬P {
      ASSERT ¬P;
      SUPPOSE P ∧ Q;
      BY conjunction_elim FROM P ∧ Q GET P;
      BY contradiction FROM P, ¬P GET FALSE;
      THEREFORE ¬(P ∧ Q);
    }
    CASE ¬Q {
      ASSERT ¬Q;
      SUPPOSE P ∧ Q;
      BY conjunction_elim FROM P ∧ Q GET Q;
      BY contradiction FROM Q, ¬Q GET FALSE;
      THEREFORE ¬(P ∧ Q);
    }
  }
  
  // Combine both directions
  CONCLUDE ¬(P ∧ Q) ↔ (¬P ∨ ¬Q);
}
```

## Implementation Components

The implementation consists of several key components:

1. **Parser**: Converts the textual representation to a logical structure
2. **Type Checker**: Validates formula well-formedness
3. **Proof Validator**: Verifies that each step follows logically from previous steps
4. **Inference Engine**: Implements logical rules of inference
5. **Theorem Database**: Stores proven theorems for future use
6. **Proof Assistant**: Suggests potential next steps or identifies gaps

### Execution Model

The proof checker validates proofs by:
1. Parsing the statements into a structured representation
2. For each step, verifying that it follows from previous steps using valid rules
3. Checking that all paths in the proof lead to the desired conclusion
4. Ensuring all cases are covered in case analysis and induction

## Future Extensions

- Interactive proof development environment
- Automated proof search for simple theorems
- Integration with external theorem provers (Coq, Isabelle, etc.)
- Support for more mathematical domains (e.g., set theory, real analysis)
- Natural language generation of proof explanations

By providing a formal yet readable language for mathematical proofs, this project bridges the gap between human-understandable arguments and machine-verifiable logic, enabling both education and advanced mathematical verification.
