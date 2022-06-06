# Managed Functions

__Managed Functions__ is a __remote management framework__ for Haskell Applications.

It enables you to easily __expose__ selected functions for remote invocation on a communication protocol.

This framework can be used to 
 - implement a custom remote management solution,
 - implement a remote procedure call (RPC) server,
 - call functions of diverse types using a uniform API,
 - simplify data serialization and deserialization.

## Architecture

Inspired by Java Management Extensions (JMX), the __Managed Functions__ framework consists of three levels:

- the __Probe__ level, where functions are encapsulated into `Probe`s,
- the __Agent__ level, where `Probe`s are composed together, forming an `Agent`,
- the __Connector__ level, where different `Connector`s can be used to expose the `Agent` API over a communication protocol.

### Probe Level

On the Probe level, functions are encapsulated in so-called Probes.
Probes expose a uniform way for calling functions of different types.
Thanks to Probes, functions of varying types can be grouped together and accessed uniformly (similarly to [Data.Dynamic](https://hackage.haskell.org/package/base/docs/Data-Dynamic.html)).

Additionally, the type annotation of a `Probe` contains information about an _encoding_ 
that should be used to deserialize input parameters and deserialize 
the output of the underlying function. 

Managed Functions implement a default encoding called `SR`, that utilizes `Show` and `Read` instances. 

`Probe` is a fairly simple data type, focused around the `call` function:

```haskell
data Probe e =
  Probe
    { call :: [In e] -> IO (Out e)
    , typeRep :: TypeRep
    }
```

Most functions can be easily converted to a Probe with `toProbe`:

```haskell
probes :: [Probe SR]
probes = [toProbe readFile, toProbe writeFile]
```

### Agent Level

On the Agent level, Probes can be grouped together into a special structure called the _Agent_.

The Agent provides a Haskell interface to list, describe, and invoke (call) Probes.

```haskell

ag :: Agent SR 
ag = fromList 
  [ ("read file", toProbe readFile)
  , ("write file", toProbe writeFile)
  ]
-- >>> invoke ag "write file" [show "/tmp/test", show "Hello World!"]
-- Right "()"

-- >>> invoke ag "read file" [show "/tmp/test"]
-- Right "\"Hello World!\""
```

### Connector Level

The purpose of the Connector level is to expose the Agent interface
outside of the Haskell language.

Unsuprisingly, the main data structure on this level is called the _Connector_.

Each Connector is specialized to a different communication protocol.
For example, there could be a `JsonRpcConnector`, 
implementing remote calls according to the [JSON-RPC](https://www.jsonrpc.org/) specification.

In terms of implementation, `Connector` is a simple newtype wrapper around the `run` function:

```haskell
newtype Connector e =
  Connector
    { run :: Agent e -> IO ()
    }
```

## Usage example

The primary purpose of __Managed Functions__ is to enable selected functions to be called remotely.

This example shows how Managed Functions can be used 
to expose the Prelude functions `readFile` and `writeFile` over HTTP.

### Full code

__package.yaml__
```yaml
dependencies:
- base 
- managed-functions
- managed-functions-http-connector
```

__Main.hs__
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

### Sample HTTP calls

Now, we can query the running HTTP server:

```shell
$ curl --json '["\"/tmp/test\"", "\"Hello World!\""]' localhost:3000/probes/write/invoke
"()"⏎

$ curl --json '["\"/tmp/test\""]' localhost:3000/probes/read/invoke
"\"Hello World!\""⏎
```
