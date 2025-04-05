# Probabilistic Programming Language

## Project Overview

This project implements a domain-specific language for expressing probabilistic models and performing statistical inference. The language allows users to define random variables, specify their distributions, express dependencies between variables, and condition on observed data to compute posterior distributions.

### Key Features

- **Random Variable Definition**: Declare variables with probability distributions
- **Conditional Distributions**: Express dependencies between variables
- **Observations**: Condition models on observed data
- **Inference Methods**: Multiple algorithms for computing posterior distributions
- **Probabilistic Queries**: Compute expectations, probabilities, and other statistics

### Applications

- Bayesian data analysis
- Machine learning model development
- Risk assessment and decision making under uncertainty
- Scientific modeling with measurement uncertainty
- Medical diagnosis and treatment planning

## Language Syntax

The language uses a combination of random variable declarations, deterministic computations, and inference directives.

### Haskell AST Definition

```haskell
-- A Program consists of a sequence of statements
data Program = Program [Statement]
  deriving (Show, Eq)

-- Statement types
data Statement
  = Sample String Distribution     -- Sample from distribution
  | Let String Expr                -- Deterministic assignment
  | Observe Distribution Expr      -- Condition on observed data
  | Factor Expr                    -- Weight current execution trace
  | If Expr [Statement] [Statement] -- Conditional execution
  | Infer InferenceMethod String [String] Int  -- Run inference
  | Return [String]                -- Return variables of interest
  deriving (Show, Eq)

-- Probability distributions
data Distribution
  = Normal Expr Expr               -- Normal distribution with mean and std
  | Uniform Expr Expr              -- Uniform distribution with min and max
  | Bernoulli Expr                 -- Bernoulli with probability p
  | Beta Expr Expr                 -- Beta distribution with alpha, beta
  | Gamma Expr Expr                -- Gamma distribution with shape, rate
  | Poisson Expr                   -- Poisson distribution with rate
  | Categorical [Expr]             -- Categorical with probabilities
  | Dirichlet [Expr]               -- Dirichlet with concentration parameters
  | Mixture [(Expr, Distribution)] -- Mixture of distributions with weights
  deriving (Show, Eq)

-- Expressions for deterministic calculations
data Expr
  = Var String                     -- Variable reference
  | Lit Value                      -- Literal value
  | Plus Expr Expr                 -- Addition
  | Minus Expr Expr                -- Subtraction
  | Times Expr Expr                -- Multiplication
  | Div Expr Expr                  -- Division
  | Exp Expr                       -- Exponential
  | Log Expr                       -- Natural logarithm
  | If' Expr Expr Expr             -- Conditional expression
  | Eq Expr Expr                   -- Equality comparison
  | Lt Expr Expr                   -- Less than comparison
  | Gt Expr Expr                   -- Greater than comparison
  | And Expr Expr                  -- Logical AND
  | Or Expr Expr                   -- Logical OR
  | Not Expr                       -- Logical NOT
  deriving (Show, Eq)

-- Value types
data Value
  = RealVal Double
  | IntVal Int
  | BoolVal Bool
  | VectorVal [Value]
  deriving (Show, Eq)

-- Inference methods
data InferenceMethod
  = MCMC Int                       -- Markov Chain Monte Carlo with iterations
  | SMC Int                        -- Sequential Monte Carlo with particles
  | Variational                    -- Variational inference
  | Enumeration                    -- Exact enumeration (for discrete models)
  deriving (Show, Eq)
```

### Example Program: Bayesian Disease Diagnosis Model

Here's a text representation of a disease diagnosis model before parsing:

```
// Bayesian Disease Diagnosis Model

// Prior probabilities for diseases
diabetes ~ Bernoulli(0.08);       // 8% prior probability
hypertension ~ Bernoulli(0.22);   // 22% prior probability

// Conditional distributions for measurements given disease state
glucose_level = if diabetes then Normal(180, 20) else Normal(90, 10);
blood_pressure = if hypertension then Normal(150, 15) else Normal(120, 10);

// Lifestyle factors
high_bmi ~ Bernoulli(0.30);
poor_diet ~ Bernoulli(0.40);

// Adjust disease probability based on lifestyle
weight = if high_bmi && poor_diet then 
           if diabetes then 2.5 else 1.0
         else 1.0;
factor(weight);

// Observed measurements
observe glucose_level ~ Normal(175, 15);
observe blood_pressure ~ Normal(155, 10);

// Inference
posterior_diabetes = infer(MCMC(10000), diabetes);
posterior_hypertension = infer(MCMC(10000), hypertension);

// Treatment recommendation
if posterior_diabetes > 0.7 then
  recommend("insulin_therapy");
end

if posterior_hypertension > 0.7 then
  recommend("antihypertensive_meds");
end

// Report results
return {
  diabetes_probability: posterior_diabetes,
  hypertension_probability: posterior_hypertension
};
```

## Implementation Components

The implementation consists of several key components:

1. **Parser**: Converts the textual representation to a probabilistic program
2. **Model Builder**: Constructs a probabilistic graphical model
3. **Inference Engine**: Implements various inference algorithms
4. **Sampler**: Generates random samples from distributions
5. **Result Analyzer**: Processes inference results into usable statistics
6. **Visualizer**: Renders probability distributions and model structure

### Execution Model

The probabilistic programming system:
1. Parses the program to build a model representation
2. Identifies random variables and their dependencies
3. Applies conditioning based on observations
4. Runs the specified inference algorithm to compute posteriors
5. Returns the requested probability distributions or statistics

Inference is typically performed using:
- **MCMC**: Markov Chain Monte Carlo methods like Metropolis-Hastings
- **SMC**: Sequential Monte Carlo (particle filtering)
- **Variational Inference**: Approximate inference using optimization
- **Exact Methods**: For small discrete models

## Future Extensions

- GPU-accelerated inference for large models
- Automatic differentiation for gradient-based inference
- Support for time-series models and dynamic systems
- Interactive visualization of inference results
- Domain-specific model libraries (epidemiology, finance, etc.)

This language makes Bayesian modeling and inference accessible to domain experts who need to quantify uncertainty in their data and predictions, without requiring deep knowledge of statistical computing algorithms.
