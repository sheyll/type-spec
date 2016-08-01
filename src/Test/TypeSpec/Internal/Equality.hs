-- | Type Equality
module Test.TypeSpec.Internal.Equality (type PolyKindEq) where

  -- | Operator 'Data.Equality.(==)' expects both arguments to have the
  -- same kind.
  type family PolyKindEq (a :: ak) (b :: bk) :: Bool where
    PolyKindEq a a = 'True
    PolyKindEq a b = 'False
