-- | Group expectations
module Test.TypeSpec.Group
    ( type (-/-), type (-*) )
  where

import Data.Kind
import Data.Proxy
import Test.TypeSpec.Core
import Test.TypeSpec.Internal.Apply
import Test.TypeSpec.Internal.Either ()
import Text.PrettyPrint

-- * Composed Expectations

-- | A @cons@ like operator.
-- Make a list of expectations. Use this to chain together any expectations
-- especially those using '(~~~)' or '(-*)', since it has a lower precedence
-- than both.
data expectation1 -/- expectation2
infixr 1 -/-

type instance
  EvalExpectation (expectation -/- expectations) =
    TyCon2 ((-/-)) <$> EvalExpectation expectation <*> EvalExpectation expectations

-- | A @cons@ like operator.
-- Make a list of expectations. Use this to chain together anything /below/ or
-- inside '(-/-)' and '(~~~)' parts. The precedence of this operator is higher
-- that that of '(-/-)'.
type expectation1 -* expectation2 = expectation1 -/- expectation2
infixr 3 -*

-- * Pretty Printing Instances

instance
    ( PrettyTypeSpec expectation1
    , PrettyTypeSpec expectation2 )
  => PrettyTypeSpec (expectation1 -/- expectation2)
  where
    prettyTypeSpec _ =
        (prettyTypeSpec pe1) $+$ (prettyTypeSpec pe2)
      where pe1 = Proxy :: Proxy expectation1
            pe2 = Proxy :: Proxy expectation2
