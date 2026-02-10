# `type-fold`

This module provides an interface for a generic folding mechanism over types,
allowing library authors to define the shape of fold, while library users can define custom folding logic for their types by implementing the `TypeFoldStep` type class.

The `TypeFold` type class serves as a marker for types that can be folded, while the `RecursiveInput` type family defines the input type for each folding step.

Let's see an example, folding `[Natural]` to `Integer`:

(The `TypeFold` instances for `[a]` is already defined by the library, so we only need to define the `TypeFoldStep` instances for our specific folding logic.)

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
import Data.Proxy
import GHC.TypeLits
import Type.Fold

instance TypeFoldStep ('[] :: [Natural]) Integer where
  foldStep _ = 0

instance KnownNat n => TypeFoldStep (n ': xs) Integer where
  foldStep = (natVal (Proxy @n) +)

example :: Integer
example = foldT @_ @[1, 2, 3]
```
