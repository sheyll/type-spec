-- | Core of the TypeSpec abstractions. Import to add custom instances.
module Test.TypeSpec.Core
  (
  -- * Core Data Type
   TypeSpec (..)
  -- * Expectations
  , type EvalExpectation
  -- * Pretty Printing Support
  , PrettyTypeSpec(..)
  , prettyIndentation
  , module ReExport
  )
  where

import Data.Proxy
import Test.TypeSpec.Internal.Either ()
import Test.TypeSpec.Internal.Apply
import Test.TypeSpec.Internal.Result as ReExport
import Text.PrettyPrint

-- | A type specification.
data TypeSpec expectation  where
  -- | Expect the given expectations to hold. If the compiler does not reject it -
  -- the expectation seem plausible.
  Valid :: (Try (EvalExpectation expectation) ~ expectation)
        => TypeSpec expectation
  -- | Expect the given expectations to **NOT** hold. If the compiler does not
  -- reject it - the expectation seem indeed implausible.
  Invalid :: (DontTry (EvalExpectation expectation))
        => TypeSpec expectation

-- | An open family of type level expectation evaluators, that return  either @()@
-- or an @ErrorMessage@.
type family EvalExpectation (expectation :: k) :: Result k

-- | Given a pair @(expectation1, expectation2)@ try to evaluate the first then,
-- if no error was returned, the second.
type instance EvalExpectation '(a, b) =
  Pair'' <$> EvalExpectation a <*> EvalExpectation b

-- | Given a list @(expectation : rest)@ try to evaluate the @expectation@ then,
-- if no error was returned, the @rest@.
type instance EvalExpectation '[] = OK '[]
type instance EvalExpectation (expectation ': rest) =
  Cons'' <$> EvalExpectation expectation <*> EvalExpectation rest


-- | A class for pretty printing via the 'Show' instance of 'TypeSpec'.
class PrettyTypeSpec (t :: k) where
  prettyTypeSpec :: proxy t -> Doc

instance PrettyTypeSpec t => Show (TypeSpec t) where
  show px@Valid =
    render
      $ hang (text "Valid:") 5 (prettyTypeSpec px)
  show px@Invalid =
    render
      $ hang (text "Invalid:") 5 (prettyTypeSpec px)

-- | The default indention to use when 'nest'ing 'Doc'uments.
prettyIndentation :: Int
prettyIndentation = 2

instance
    ( PrettyTypeSpec expectation1
    , PrettyTypeSpec expectation2 )
  => PrettyTypeSpec '(expectation1, expectation2)
  where
    prettyTypeSpec _ =
        prettyTypeSpec pe1 $$ prettyTypeSpec pe2
      where pe1 = Proxy :: Proxy expectation1
            pe2 = Proxy :: Proxy expectation2

instance
    PrettyTypeSpec '[]
  where
    prettyTypeSpec _ = empty

instance
    ( PrettyTypeSpec expectation
    , PrettyTypeSpec rest )
  => PrettyTypeSpec (expectation ': rest)
  where
    prettyTypeSpec _ =
        (prettyTypeSpec pe1) $$ (prettyTypeSpec pe2)
      where pe1 = Proxy :: Proxy expectation
            pe2 = Proxy :: Proxy rest
