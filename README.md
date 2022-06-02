# Managed Functions

__Managed Functions__ is a __remote management framework__ for Haskell Applications.

It enables you to easily __expose__ selected functions for remote invocation.

## Architecture

Inspired by Java Management Extensions (JMX), the __Managed Functions__ framework consists of three levels:

- the __Probe__ level, where functions are encapsulated into `Probe`s,
- the __Agent__ level, where `Probe`s are composed together forming an `Agent`,
- the __Connector__ level, where different `Connector`s can be used to expose the `Agent` API over a communication protocol.


## Usage
### Basic usage

This guide shows how __Managed Functions__ can be used
to remotely invoke the Prelude functions `readFile` and `writeFile`.

```haskell
import Managed
import Managed.Connectors.HTTPConnector

ag :: Agent SR
ag = fromList  
    [ ("read", toProbe readFile)
    , ("write", toProbe writeFile)
    ]

main :: IO ()
main = run httpConnector ag
```

### Creating an Encoding
### Creating a Connector
