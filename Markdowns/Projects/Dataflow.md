# Dataflow Programming Language

## Project Overview
This project implements a simplified domain-specific language for expressing data processing pipelines as directed graphs of operations. The language enables users to define data transformations declaratively, allowing the system to handle execution details automatically.

## Key Goals
1. **Parser Implementation**: Convert textual pipeline definitions into a directed graph representation
2. **Graph Analyzer & Validator**: Ensure graph correctness, type compatibility, and detect cycles
3. **Test Suite**: Comprehensive testing of language features and execution correctness

## Simplified Syntax Definition

```haskell
-- A Program is a collection of nodes and their connections
data Program = Program [Node] [Edge]
  deriving (Show, Eq)

-- Simplified data types for type-checking connections
data Type 
  = ImageType
  | NumericType
  | TableType String [Column]
  | StreamType Type
  deriving (Show, Eq)

data Column = Column String Type
  deriving (Show, Eq)

-- Node definitions for different operations
data Node 
  = Source String Type [Parameter]             -- Data source node
  | Sink String Type [Parameter]               -- Data output node
  | Transform String String [Parameter]        -- Transformation function
  | Filter String String [Parameter]           -- Filtering operation
  | Merge String [Parameter]                   -- Combine multiple inputs
  | Split String [Parameter]                   -- Distribute to multiple outputs
  deriving (Show, Eq)

-- Edge connects output port of one node to input port of another
data Edge = Edge String String                 -- From node, to node
  deriving (Show, Eq)

-- Parameter definition
data Parameter = Parameter String Value
  deriving (Show, Eq)

data Value
  = StringValue String
  | NumberValue Double
  | BoolValue Bool
  | ListValue [Value]
  deriving (Show, Eq)
```

## Example Pipeline
```
// Simple Image Processing Pipeline
SOURCE camera: ImageStream {
  width: 1280,
  height: 720,
  format: "rgb24"
}

TRANSFORM resize FROM camera {
  width: 640,
  height: 480,
  method: "bilinear"
}

TRANSFORM grayscale FROM resize {
  weights: [0.299, 0.587, 0.114]
}

TRANSFORM blur FROM grayscale {
  kernelSize: 3,
  sigma: 1.5
}

SINK display FROM blur {
  windowName: "Processed Image"
}

SINK fileOutput FROM blur {
  path: "/output/",
  format: "png"
}
```

## Implementation Components

### 1. Parser
- Process pipeline definitions into a graph structure
- Handle node declarations and connections
- Parse parameters and type specifications
- Support comments and basic error recovery
- Generate meaningful error messages for syntax issues

### 2. Graph Analyzer & Validator
- Build internal representation of the dataflow graph
- Verify graph connectivity (no isolated nodes)
- Detect cycles in the graph that would prevent execution
- Perform type checking between connected nodes
- Validate parameter correctness for each node type

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Graph validation operations
  - Type compatibility checking
- **Property-Based Testing**:
  - Generate random valid and invalid graphs
  - Test graph properties (connectivity, acyclicity)
