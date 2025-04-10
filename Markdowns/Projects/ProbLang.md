# Probabilistic Programming Language

## Project Overview
This project implements a simplified domain-specific language for expressing probabilistic models and performing statistical inference. The language allows users to define random variables, specify their distributions, express dependencies between variables, and condition on observed data to infer posterior distributions.

## Key Goals
1. **Parser Implementation**: Convert probabilistic program definitions into an AST
2. **Model Builder & Inference Engine**: Construct probabilistic models and implement sampling-based inference
3. **Test Suite**: Ensure correctness through unit and property-based testing

## Simplified Syntax Definition

```haskell
-- A Program consists of a sequence of statements
data Program = Program [Statement]
  deriving (Show, Eq)

-- Statement types
data Statement
  = Sample String Distribution     -- Sample from distribution
  | Let String Expr                -- Deterministic assignment
  | Observe Distribution Expr      -- Condition on observed data
  | If Expr [Statement] [Statement] -- Conditional execution
  | Infer InferenceMethod String   -- Run inference
  | Return [String]                -- Return variables of interest
  deriving (Show, Eq)

-- Probability distributions
data Distribution
  = Normal Expr Expr               -- Normal distribution with mean and std
  | Uniform Expr Expr              -- Uniform distribution with min and max
  | Bernoulli Expr                 -- Bernoulli with probability p
  | Beta Expr Expr                 -- Beta distribution with alpha, beta
  | Categorical [Expr]             -- Categorical with probabilities
  deriving (Show, Eq)

-- Expressions for deterministic calculations
data Expr
  = Var String                     -- Variable reference
  | Lit Value                      -- Literal value
  | Plus Expr Expr                 -- Addition
  | Minus Expr Expr                -- Subtraction
  | Times Expr Expr                -- Multiplication
  | Div Expr Expr                  -- Division
  | If' Expr Expr Expr             -- Conditional expression
  | Eq Expr Expr                   -- Equality comparison
  | Lt Expr Expr                   -- Less than comparison
  | And Expr Expr                  -- Logical AND
  | Or Expr Expr                   -- Logical OR
  deriving (Show, Eq)

-- Value types
data Value
  = RealVal Double
  | IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)

-- Inference methods
data InferenceMethod
  = MCMC Int                       -- Markov Chain Monte Carlo with iterations
  | Rejection Int                  -- Rejection sampling with iterations
  deriving (Show, Eq)
```

## Example Program: Medical Diagnosis Model

```
// Bayesian medical diagnosis model

// Prior probabilities for disease
has_disease ~ Bernoulli(0.01);  // 1% of population has the disease

// Test accuracy parameters
true_positive_rate = 0.95;  // Sensitivity
false_positive_rate = 0.10; // 1 - Specificity

// Model for test result based on disease status
test_result = if has_disease then
                // Patient has disease - test is positive with 95% probability
                Bernoulli(true_positive_rate)
              else
                // Patient doesn't have disease - test is positive with 10% probability
                Bernoulli(false_positive_rate);

// Observe that the test came back positive
observe test_result ~ 1;

// Calculate posterior probability of disease given positive test
posterior = infer(MCMC(10000), has_disease);

// Return results
return [has_disease];
```

In this medical diagnosis example:
1. We start with a prior belief that 1% of the population has the disease
2. We model the test's accuracy with sensitivity and specificity parameters
3. We observe a positive test result
4. We use MCMC to compute the posterior probability of having the disease
5. The result demonstrates Bayes' theorem in action, showing how the positive test result updates our belief about disease status

## Implementation Components

### 1. Parser
- Process probabilistic program definitions into AST
- Handle variable declarations and assignments
- Support distribution definitions and observations
- Generate meaningful error messages for syntax issues
- Support comments and basic error recovery

### 2. Model Builder & Inference Engine
- Construct internal representation of the probabilistic model
- Track random variables and their dependencies
- Implement sampling from basic distributions
- Support Metropolis-Hastings MCMC for inference
- Handle observed data and conditioning
- Generate and analyze samples from posterior distributions

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Distribution sampling validation
  - Basic inference on simple models
  - Conditional probability calculations
  - Expression evaluation
  - Model construction correctness
  
- **Property-Based Testing**:
  - Generate random valid probabilistic programs
  - Test statistical properties of samplers
