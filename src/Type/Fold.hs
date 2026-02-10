{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if __GLASGOW_HASKELL__ >= 910
{-# LANGUAGE RequiredTypeArguments  #-}
#endif
-- | This module provides an interface for a generic folding mechanism over types,
-- allowing library authors to define the shape of fold, while library users can define custom folding logic
-- for their types by implementing the `TypeFoldStep` type class.
--
-- The `TypeFold` type class serves as a marker for types that can be folded, while the `RecursiveInput`
-- type family defines the input type for each folding step.
--
-- The `foldStep_` and `foldT_` functions provide convenient ways to invoke the folding logic defined by the user.
module Type.Fold where

import Data.Kind (Type)

-- | t is the type being folded (which can be any kind, e.g. a type-level list, a type-level constructor, etc.)
--
-- r is the result value type of the fold (which is a concrete value)
class TypeFold (t :: k) (r :: Type) where
  foldT :: r

-- | A type family that defines the input type for each folding step
--
-- typically includes all the returned values from folding the children
type family   RecursiveInput (t :: k) (r :: Type) :: Type

-- | Meant to be implemented by downstream users to provide custom folding logic
class TypeFoldStep (t :: k) (r :: Type) where
  foldStep :: RecursiveInput t r -> r

#if __GLASGOW_HASKELL__ >= 910
-- | Helper function that uses RequiredTypeArguments
--
-- exists if ghc >= 9.10
foldStep_ :: forall t -> TypeFoldStep t r => RecursiveInput t r -> r
foldStep_ t = foldStep @_ @t
{-# INLINE foldStep_ #-}

-- | Helper function that uses RequiredTypeArguments
--
-- exists if ghc >= 9.10
foldT_ :: forall t -> TypeFold t r => r
foldT_ t = foldT @_ @t
{-# INLINE foldT_ #-}
#endif

type instance RecursiveInput '[]      r = ()
type instance RecursiveInput (x : xs) r = r

-- | Nil case, the user provides an 'r'
instance TypeFoldStep ('[] :: [a]) r => TypeFold ('[] :: [a]) r where
  foldT = foldStep @_ @('[] :: [a]) ()
  {-# INLINE foldT #-}

-- | Think of the 'TypeFoldStep (x : xs) r' as 'a -> r -> r'.
-- The user provides an 'r -> r' based on the type 'x'.
instance (TypeFoldStep (x ': xs :: [a]) r, TypeFold xs r) => TypeFold (x ': xs :: [a]) r where
  foldT = foldStep @_ @(x : xs :: [a]) $ foldT_ xs
  {-# INLINE foldT #-}
