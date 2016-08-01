-- | Useful abstractions for type level programming using 'Either'.
module Test.TypeSpec.Internal.Either (type FromLeft) where

import Test.TypeSpec.Internal.Apply 

-- * Either instances

type instance (>>=) ('Right a) f = Apply f a
type instance (>>=) ('Left b)  f = 'Left b

type instance (<$>) f ('Right a) = 'Right (Apply f a)
type instance (<$>) f ('Left a) = 'Left a

type family
  FromLeft (e :: Either a b) :: a where
  FromLeft ('Left a) = a
