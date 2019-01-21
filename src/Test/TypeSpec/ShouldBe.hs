-- | Type level assertions on type equality.
module Test.TypeSpec.ShouldBe
    (   ShouldBe
      , ShouldNotBe
      , ShouldBeTrue
      , ShouldBeFalse
      , ButNot
    )
  where

import Data.Kind
import Data.Type.Bool
import GHC.TypeLits
import Test.TypeSpec.Core
import Test.TypeSpec.Internal.Apply ()
import Test.TypeSpec.Internal.Either ()
import Test.TypeSpec.Internal.Equality
import Text.PrettyPrint

-- | State that a type is equal to the type level @True@.
data ShouldBeTrue :: expectation -> Type

type instance EvalExpectation (ShouldBeTrue t) =
    If (EqExtra t 'True)
        (OK (ShouldBeTrue t))
        (FAILED
          ('Text "Should have been 'True: " ':<>: 'ShowType t))

-- | State that a type is equal to the type level @False@.
data ShouldBeFalse :: expectation -> Type

type instance EvalExpectation (ShouldBeFalse t) =
    If (EqExtra t 'False)
        (OK (ShouldBeFalse t))
        (FAILED
          ('Text "Should have been 'False: " ':<>: 'ShowType t))

-- | State that one type is different to two other types. This must always be
-- used right next to a 'ShouldBe' pair, otherwise this will not work.
data ButNot :: shouldBe -> shouldntBe -> Type

type instance
  EvalExpectation (ButNot (ShouldBe actual expected) other) =
    If (EqExtra actual expected)
      (If (EqExtra other expected)
          (FAILED
            ('Text "Expected type: "
             ':$$: 'Text "   " ':<>: 'ShowType expected
             ':$$: 'Text "to be different from: "
             ':$$: 'Text "   " ':<>: 'ShowType other))
          (OK (ButNot (ShouldBe actual expected) other)))
      (FAILED
        ('Text "Expected type: " ':<>: 'ShowType expected
         ':$$: 'Text "Actual type:   " ':<>: 'ShowType actual))

-- | State that two types or type constructs are boiled down to the same type.
data ShouldBe :: actual -> expected -> Type

type instance
  EvalExpectation (ShouldBe actual expected) =
    If (EqExtra actual expected)
        (OK (ShouldBe actual expected))
        (FAILED
          ('Text "Expected type: " ':<>: 'ShowType expected
           ':$$: 'Text "Actual type:   " ':<>: 'ShowType actual))

-- | State that two types or type constructs are NOT the same type.
data ShouldNotBe :: actual -> expected -> Type

type instance
  EvalExpectation (ShouldNotBe actual expected) =
    If (EqExtra expected actual)
        (FAILED
          ('Text "Expected type: "
           ':$$: 'Text "   " ':<>: 'ShowType expected
           ':$$: 'Text "to be different from: "
           ':$$: 'Text "   " ':<>: 'ShowType actual))
        (OK (ShouldNotBe actual expected))

instance PrettyTypeSpec (ShouldBeTrue a) where
  prettyTypeSpec _px =
    prettyCheck "True"

instance PrettyTypeSpec (ShouldBeFalse a) where
  prettyTypeSpec _px =
    prettyCheck "False"

instance PrettyTypeSpec (ShouldBe a b) where
  prettyTypeSpec _px =
    prettyCheck "Equal"

instance PrettyTypeSpec (ShouldNotBe a b) where
  prettyTypeSpec _px =
      prettyCheck "Different"

instance
    (a ~ (ShouldBe a0 a1))
  => PrettyTypeSpec (ButNot a b) where
    prettyTypeSpec _ =
      prettyCheck  "Restricted"

-- | Pretty print a test prefix by a bullet-point.
prettyCheck :: String -> Doc
prettyCheck doc = parens (text "OK" <+> text doc)
