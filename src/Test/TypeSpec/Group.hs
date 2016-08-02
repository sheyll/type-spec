-- | Composed Expectations.
module Test.TypeSpec.Group
    ( type (-/-) )
  where

import Data.Proxy
import Test.TypeSpec.Core
import Test.TypeSpec.Internal.Apply
import Test.TypeSpec.Internal.Either ()
import Text.PrettyPrint

{-| Combine two expectations.

Make a collection of expectations:

>   (2 + 2)  `Is`     4
>                           -/-
>   (4 + 4)  `Is`     8
>                           -/-
>   'True    `IsNot` 'False
-}
data expectation1 -/- expectation2
infixr 1 -/-

type instance
  EvalExpectation (expectation -/- expectations) =
    TyCon2 (-/-) <$> EvalExpectation expectation <*> EvalExpectation expectations

-- | Pretty Printing Instance.
instance
    ( PrettyTypeSpec expectation1
    , PrettyTypeSpec expectation2 )
  => PrettyTypeSpec (expectation1 -/- expectation2)
  where
    prettyTypeSpec _ =
        prettyTypeSpec pe1 $+$ prettyTypeSpec pe2
      where pe1 = Proxy :: Proxy expectation1
            pe2 = Proxy :: Proxy expectation2
