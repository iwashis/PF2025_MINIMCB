# Reactive Systems Language

## Project Overview

This project implements a domain-specific language for describing reactive systems as state machines with event-driven transitions. The language provides abstractions for defining components, states, transitions, and actions, making it particularly well-suited for systems that respond to external events and maintain complex state.

### Key Features

- **Component-Based Design**: Define systems as interconnected components
- **State Machine Model**: Express behavior using states and transitions
- **Event-Driven Architecture**: React to events from various sources
- **Conditional Transitions**: Specify guards and conditions for state changes
- **Action Definitions**: Associate behavior with states and transitions

### Applications

- Embedded control systems
- Smart home automation
- IoT device programming
- User interface controllers
- Protocol implementations
- Industrial automation systems

## Language Syntax

The language uses a structured approach to defining reactive components with their states and transitions.

### Haskell AST Definition

```haskell
-- A Program is a collection of components
data Program = Program [Component]
  deriving (Show, Eq)

-- Component definition with states, transitions, and default actions
data Component = Component String [State] [Transition] [Action]
  deriving (Show, Eq)

-- State definition with entry/exit actions
data State = State 
  { stateName :: String
  , entryActions :: [Action]
  , exitActions :: [Action]
  , stateActions :: [Action]
  }
  deriving (Show, Eq)

-- Transition between states
data Transition = Transition 
  { fromState :: String
  , toState :: String
  , triggerEvent :: Event
  , conditions :: [Condition]
  , transitionActions :: [Action]
  }
  deriving (Show, Eq)

-- Event types that can trigger transitions
data Event
  = Signal String                  -- Named signal
  | Timeout Int                    -- Time-based event (milliseconds)
  | Message String                 -- Message receipt
  | PeriodicEvent Int              -- Recurring event
  | CompositeEvent [Event] String  -- Combination of events (AND/OR)
  deriving (Show, Eq)

-- Condition for guarded transitions
data Condition = Condition String [Expr]
  deriving (Show, Eq)

-- Actions to perform
data Action = Action String [Expr]
  deriving (Show, Eq)

-- Expressions for conditions and action parameters
data Expr
  = VarRef String                  -- Variable reference
  | Literal Value                  -- Literal value
  | Plus Expr Expr                 -- Addition
  | Minus Expr Expr                -- Subtraction
  | Times Expr Expr                -- Multiplication
  | Div Expr Expr                  -- Division
  | Eq Expr Expr                   -- Equality comparison
  | Gt Expr Expr                   -- Greater than
  | Lt Expr Expr                   -- Less than
  | And Expr Expr                  -- Logical AND
  | Or Expr Expr                   -- Logical OR
  | Not Expr                       -- Logical NOT
  | FunctionCall String [Expr]     -- Function call
  deriving (Show, Eq)

-- Value types
data Value
  = IntVal Int
  | FloatVal Double
  | BoolVal Bool
  | StringVal String
  | ListVal [Value]
  deriving (Show, Eq)
```

### Example Program: Smart Home Controller

Here's a text representation of a smart home controller system before parsing:

```
// Smart Home Controller System

COMPONENT MainController {
  STATES {
    Idle {
      DO SetLightsAuto(true);
    }
    
    Away {
      DO SetLightsOff();
      DO SetAlarmOn();
    }
    
    Night {
      DO SetLightsLow();
      DO SetDoorsLocked();
    }
    
    Morning {
      DO SetLightsBright();
      DO SetCoffeeMachine(true);
    }
  }
  
  TRANSITIONS {
    Idle -> Away ON LeaveHome {
      DO NotifyUser("Home secured");
    }
    
    Away -> Idle ON ReturnHome WHEN AuthenticateUser() {
      DO SetAlarmOff();
      DO AdjustTemperature(22);
    }
    
    Idle -> Night ON NightMode {
      DO SetTimeout(28800, MorningWakeup);  // 8 hours
    }
    
    Night -> Morning ON MorningWakeup {
      DO OpenBlinds();
      DO PlayMusic("WakeupPlaylist");
    }
    
    Morning -> Idle ON Timeout(3600) {
      // Automatic transition after 1 hour
    }
  }
  
  // Default action for all transitions
  DEFAULT_ACTION {
    DO LogStateChange();
  }
}

COMPONENT MotionSensor {
  STATES {
    NoMotion {}
    MotionDetected {
      DO LogMotion();
    }
  }
  
  TRANSITIONS {
    NoMotion -> MotionDetected ON MotionDetected {
      DO Emit(LightsOn);
    }
    
    MotionDetected -> NoMotion ON Timeout(300) {
      DO Emit(LightsOff);
    }
  }
}

COMPONENT TemperatureController {
  STATES {
    Heating {
      DO SetHeater(true);
    }
    
    Cooling {
      DO SetAC(true);
    }
    
    Stable {
      DO SetHeater(false);
      DO SetAC(false);
    }
  }
  
  TRANSITIONS {
    Stable -> Heating ON TemperatureReading WHEN BelowTarget(2) {
      DO NotifyUser("Heating started");
    }
    
    Stable -> Cooling ON TemperatureReading WHEN AboveTarget(2) {
      DO NotifyUser("Cooling started");
    }
    
    Heating -> Stable ON TemperatureReading WHEN AtTarget() {}
    
    Cooling -> Stable ON TemperatureReading WHEN AtTarget() {}
  }
  
  DEFAULT_ACTION {
    DO LogTemperature();
  }
}
```

## Implementation Components

The implementation consists of several key components:

1. **Parser**: Converts textual representation to state machine model
2. **Validator**: Checks for consistency and completeness
3. **Event Dispatcher**: Manages event delivery to components
4. **State Manager**: Tracks current states and handles transitions
5. **Action Executor**: Performs actions associated with states and transitions
6. **Simulator**: Tests system behavior with simulated events
7. **Code Generator**: Produces executable code for target platforms

### Execution Model

The reactive system operates by:
1. Initializing all components to their initial states
2. Waiting for events from external sources or timers
3. Upon event receipt:
   - Checking if any transitions are triggered by the event
   - Evaluating transition conditions
   - If conditions are met, executing exit actions for the current state
   - Executing transition actions
   - Transitioning to the new state
   - Executing entry actions for the new state
4. Continuing to process events indefinitely
