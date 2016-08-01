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
import Data.Type.Equality
import Data.Typeable
import GHC.TypeLits
import Test.TypeSpec.Core
import Test.TypeSpec.Internal.Apply ()
import Test.TypeSpec.Internal.Either ()
import Test.TypeSpec.Internal.Equality
import Text.PrettyPrint
import Type.Showtype

-- | State that a type is equal to the type level @True@.
data ShouldBeTrue :: expectation -> Type

type instance EvalExpectation (ShouldBeTrue t) =
    If (PolyKindEq t 'True)
        (OK (ShouldBeTrue t))
        (FAILED
          ('Text "Should have been 'True: " ':<>: 'ShowType t))

-- | State that a type is equal to the type level @False@.
data ShouldBeFalse :: expectation -> Type

type instance EvalExpectation (ShouldBeFalse t) =
    If (PolyKindEq t 'False)
        (OK (ShouldBeFalse t))
        (FAILED
          ('Text "Should have been 'False: " ':<>: 'ShowType t))

-- | State that one type is different to two other types. This must always be
-- used right next to a 'ShouldBe' pair, otherwise this will not work.
data ButNot :: shouldBe -> actual -> Type

type instance
  EvalExpectation (ButNot (ShouldBe expected actual) other) =
    If (expected == actual)
      (If (expected == other)
          (FAILED
            ('Text "Expected type: "
             ':$$: 'Text "   " ':<>: 'ShowType expected
             ':$$: 'Text "to be different from: "
             ':$$: 'Text "   " ':<>: 'ShowType other))
          (OK (ButNot (ShouldBe expected actual) other)))
      (FAILED
        ('Text "Expected type: " ':<>: 'ShowType expected
         ':$$: 'Text "Actual type:   " ':<>: 'ShowType actual))

-- | State that two types or type constructs are boiled down to the same type.
data ShouldBe :: expected -> actual -> Type

type instance
  EvalExpectation (ShouldBe expected actual) =
    If (PolyKindEq expected actual)
        (OK (ShouldBe expected actual))
        (FAILED
          ('Text "Expected type: " ':<>: 'ShowType expected
           ':$$: 'Text "Actual type:   " ':<>: 'ShowType actual))

-- | State that two types or type constructs are NOT the same type.
data ShouldNotBe :: expected -> actual -> Type

type instance
  EvalExpectation (ShouldNotBe expected actual) =
    If (PolyKindEq expected actual)
        (FAILED
          ('Text "Expected type: "
           ':$$: 'Text "   " ':<>: 'ShowType expected
           ':$$: 'Text "to be different from: "
           ':$$: 'Text "   " ':<>: 'ShowType actual))
        (OK (ShouldNotBe expected actual))

-- * PrettyTypeSpec instances

instance PrettyTypeSpec (ShouldBeTrue a) where
  prettyTypeSpec _px =
    prettyBulletPoint $ text "Type == True"

instance PrettyTypeSpec (ShouldBeFalse a) where
  prettyTypeSpec _px =
    prettyBulletPoint $ text "Type == False"

instance PrettyTypeSpec (ShouldBe a b) where
  prettyTypeSpec _px =
    prettyBulletPoint $ text "Types are equal"

instance (Showtype a, Showtype b) => PrettyTypeSpec (ShouldNotBe a b) where
  prettyTypeSpec _px =
      prettyBulletPoint
        $ sentence "Type"  (text expected)
        $$ sentence "differs from" (text actual)
    where
      expected = showtype (Proxy :: Proxy a)
      actual = showtype (Proxy :: Proxy b)

instance
    (a ~ (ShouldBe a0 a1)
    , Showtype a0
    , Showtype a1
    , Showtype b )
  => PrettyTypeSpec (ButNot a b) where
    prettyTypeSpec _ =
      prettyBulletPoint
        $ (sentence "Type"
                 (text (showtype (Proxy :: Proxy a0))))
        $$ (sentence "is equal to"
                 (text (showtype (Proxy :: Proxy a1))))
        $$ (sentence "but not to"
                 (text (showtype (Proxy :: Proxy b))))

-- | Pretty print a test prefix by a bullet-point.
prettyBulletPoint :: Doc -> Doc
prettyBulletPoint doc = text "•" <+> doc 
