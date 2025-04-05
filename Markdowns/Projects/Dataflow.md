# Dataflow Programming Language

## Project Overview

This project implements a domain-specific language for expressing data processing pipelines as directed graphs of operations. The language is designed for applications that naturally fit a dataflow paradigm, such as image processing, signal processing, data analytics, and stream processing.

### Key Features

- **Declarative Pipeline Definition**: Express data transformations as a network of connected components
- **Type-Safe Connections**: Validate compatibility between data producers and consumers
- **Parallel Execution**: Automatically identify and exploit parallelism opportunities
- **Streaming Processing**: Handle continuous data flows with minimal buffering
- **Composable Transformations**: Build complex pipelines from reusable components

### Applications

- Image and video processing pipelines
- ETL (Extract, Transform, Load) workflows for data processing
- Real-time analytics on streaming data
- Signal processing in telecommunications and audio systems
- IoT sensor data processing and analytics

## Language Syntax

The language uses a declarative style where nodes and connections are specified separately. Each node has a type, optional parameters, and connection specifications.

### Haskell AST Definition

```haskell
-- A Program is a collection of nodes and their connections
data Program = Program [Node] [Edge]
  deriving (Show, Eq)

-- Data types for type-checking connections
data Type 
  = ImageType
  | SignalType
  | NumericType
  | TableType String [Column]
  | StreamType Type
  | CustomType String
  deriving (Show, Eq)

data Column = Column String Type
  deriving (Show, Eq)

-- Node definitions for different operations
data Node 
  = Source String Type [Parameter]             -- Data source node
  | Sink String Type [Parameter]               -- Data output node
  | Transform String Function [Parameter]      -- Transformation function
  | Filter String Predicate [Parameter]        -- Filtering operation
  | Merge String [Parameter]                   -- Combine multiple inputs
  | Split String [Parameter]                   -- Distribute to multiple outputs
  | Aggregate String Function [Parameter]      -- Aggregation operation
  | Custom String [Parameter]                  -- User-defined node
  deriving (Show, Eq)

-- Edge connects output port of one node to input port of another
data Edge = Edge String String (Maybe String) (Maybe String)  -- From node, to node, output port, input port
  deriving (Show, Eq)

-- Function and parameter definitions
data Function = Function String
  deriving (Show, Eq)

data Predicate = Predicate String
  deriving (Show, Eq)

data Parameter = Parameter String Value
  deriving (Show, Eq)

data Value
  = StringValue String
  | NumberValue Double
  | BoolValue Bool
  | ListValue [Value]
  deriving (Show, Eq)
```

### Example Program: Image Processing Pipeline

Here's a text representation of an image processing pipeline before parsing:

```
// Image Processing Pipeline
SOURCE camera: ImageStream {
  width: 1920,
  height: 1080,
  format: "rgb24",
  fps: 30
}

TRANSFORM resize FROM camera {
  width: 640,
  height: 480,
  method: "bilinear"
}

TRANSFORM grayscale FROM resize {
  weights: [0.299, 0.587, 0.114]
}

TRANSFORM edgeDetect FROM grayscale {
  algorithm: "sobel",
  threshold: 50
}

TRANSFORM threshold FROM edgeDetect {
  value: 127,
  maxValue: 255,
  type: "binary"
}

FILTER motionFilter FROM threshold {
  sensitivity: 0.3,
  minArea: 100
}

TRANSFORM annotate FROM motionFilter {
  timestamp: true,
  location: true,
  fontScale: 0.5
}

TRANSFORM compress FROM annotate {
  format: "jpeg",
  quality: 80
}

SINK display FROM compress {
  windowName: "Motion Detection"
}

SINK storage FROM compress {
  path: "/recordings/",
  retention: "48h",
  prefix: "motion_"
}
```

## Implementation Components

The implementation consists of several key components:

1. **Parser**: Converts the textual representation to a graph representation
2. **Graph Analyzer**: Validates the dataflow graph (connectivity, type compatibility)
3. **Scheduler**: Determines an efficient execution order
4. **Executor**: Manages the dataflow execution
5. **Optimizer**: Improves pipeline performance through transformations
6. **Visualizer**: Renders the pipeline as a graph diagram

### Execution Model

The dataflow engine executes the pipeline by:
1. Topologically sorting the graph to determine execution order
2. Creating execution threads for parallel paths
3. Establishing communication channels between nodes
4. Pushing data through the pipeline, allowing concurrent execution of independent paths

## Future Extensions

- Visual editor for pipeline construction
- Dynamic reconfiguration of pipelines at runtime
- Distributed execution across multiple machines
- Domain-specific node libraries for various applications
- Automatic optimization based on runtime profiling

This language makes complex data processing workflows more manageable by expressing them as interconnected components with clear data dependencies, enabling automatic parallelization and optimization.
