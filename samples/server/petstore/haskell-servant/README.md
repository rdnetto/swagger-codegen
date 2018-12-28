# Auto-Generated Swagger Bindings to `SwaggerPetstore`

The library in `lib` provides auto-generated-from-Swagger bindings to the SwaggerPetstore API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

## Using as a Client

Calls to endpoints are defined in the `SwaggerPetstore.Client` module, as functions in a monad satisfying the `SwaggerPetstoreClientMonad` typeclass.
This is typically satisfied by the `SwaggerPetstoreClient` monad (and associated transformer) from the `SwaggerPetstore.ClientT` module.

To actually run the monad with `runClient`, you will need to provide a hostname and port, like so:

```haskell
import SwaggerPetstore.Client
import SwaggerPetstore.ClientT
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client (BaseUrl(..), Scheme(Http), mkClientEnv)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" 80 "/"
      clientEnv = mkClientEnv manager baseUrl

  res <- runClient clientEnv $ do
    -- Any SwaggerPetstore API call can go here.
    return ()

  case res of
       Left err -> print err
       Right () -> return ()
```

## Creating a Server

In order to create a server, you must use the `runSwaggerPetstoreServer` function. To
specify the handling of the different endpoints, you should provide a
`SwaggerPetstoreBackend`. For example, if you have defined handler functions for all
the functions in `SwaggerPetstore.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import SwaggerPetstore.Server

-- A module you wrote yourself, containing all handlers needed for the SwaggerPetstoreBackend type.
import SwaggerPetstore.Handlers

-- Run a SwaggerPetstore server on localhost:8080
main :: IO ()
main = do
  let server = SwaggerPetstoreBackend{..}
  runSwaggerPetstoreServer "localhost" 8080 server
```

