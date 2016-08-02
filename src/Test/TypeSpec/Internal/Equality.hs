-- | Type Equality
module Test.TypeSpec.Internal.Equality (type EqExtra) where

  import Data.Type.Equality

  -- | Operator 'Data.Equality.(==)' expects both arguments to have the
  -- same kind.
  type family EqExtra (a :: ak) (b :: bk) :: Bool where
    EqExtra ('Left x) ('Left y) = EqExtra x y
    EqExtra ('Right x) ('Right y) = EqExtra x y
    EqExtra a a = 'True
    EqExtra (a :: k) (b :: k) = a == b
    EqExtra a b = 'False
