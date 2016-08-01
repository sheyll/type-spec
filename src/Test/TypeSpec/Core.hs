-- | Core of the TypeSpec abstractions. Import to add custom instances.
module Test.TypeSpec.Core
  ( TypeSpec (..)
  , type EvalExpectation
  , PrettyTypeSpec(..)
  , prettyIndentation
  , nest'
  , sentence
  , module ReExport
  )
  where

import Data.Proxy
import Test.TypeSpec.Internal.Either ()
import Test.TypeSpec.Internal.Apply
import Test.TypeSpec.Internal.Result as ReExport
import Text.PrettyPrint

-- * Type Level Specifcations

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

-- * Expectations

-- | An open family of type level expectation evaluators, that return  either @()@
-- or an @ErrorMessage@.
type family EvalExpectation (expectation :: k) :: Result k

-- ** Combining/Grouping collections

-- | Given a pair @(expectation1, expectation2)@ try to evaluate the first then,
-- if no error was returned, the second.
type instance EvalExpectation '(a, b) =
  Pair'' <$> EvalExpectation a <*> EvalExpectation b

-- | Given a list @(expectation : rest)@ try to evaluate the @expectation@ then,
-- if no error was returned, the @rest@.
type instance EvalExpectation '[] = OK '[]
type instance EvalExpectation (expectation ': rest) =
  Cons'' <$> EvalExpectation expectation <*> EvalExpectation rest

-- * Pretty Printing

-- | A class for pretty printing via the 'Show' instance of 'TypeSpec'.
class PrettyTypeSpec (t :: k) where
  prettyTypeSpec :: proxy t -> Doc

-- * Printing/Showing

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

-- | Nest using the default indention `prettyIndentation`.
nest' :: Doc -> Doc
nest' = nest prettyIndentation

-- | Print a /sentence/ with the second part 'hang'ing from the first.
-- Generate: @predicate: object@
sentence :: String -> Doc -> Doc
sentence predicate object =
  hang (text predicate <> colon <> space) 5 object

-- * Default instances

instance
    ( PrettyTypeSpec expectation1
    , PrettyTypeSpec expectation2 )
  => PrettyTypeSpec '(expectation1, expectation2)
  where
    prettyTypeSpec _ =
        prettyTypeSpec pe1 $+$ prettyTypeSpec pe2 
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
        (prettyTypeSpec pe1) $+$ (prettyTypeSpec pe2)
      where pe1 = Proxy :: Proxy expectation
            pe2 = Proxy :: Proxy rest
