-- | Label expectations
module Test.TypeSpec.Label
    ( It )
  where

import Data.Kind
import Data.Typeable
import GHC.TypeLits
import Test.TypeSpec.Core
import Test.TypeSpec.Internal.Apply
import Test.TypeSpec.Internal.Either ()
import Text.PrettyPrint

-- | Add a type level string as label or longer descripton around expectations.
-- This is analog to the @it@ function in the @hspec@ package.
data It :: Symbol -> expectation -> Type

type instance
  EvalExpectation (It message expectation) =
    PrependToError
      ('Text message ':$$: 'Text "    ")
      (EvalExpectation expectation)
    >> (OK (It message expectation))


instance (KnownSymbol msg, PrettyTypeSpec x) => PrettyTypeSpec (It msg x) where
  prettyTypeSpec _ =
    (text (symbolVal (Proxy :: Proxy msg)))
    $+$ nest prettyIndentation (prettyTypeSpec (Proxy :: Proxy x))
