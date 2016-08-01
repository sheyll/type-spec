-- | A tiny EDSL to write type-level-unit tests.
module Test.TypeSpec
  ( type Expect
  , type Explain
  , type It's
  , type IsTheSameAs
  , type Is
  , type TheseAreEqual
  , type IsNot
  , type Isn't
  , type IsNotTheSameAs
  , type IsDifferentFrom
  , type TheseAreNotEqual
  , type IsTrue
  , type Therefore
  , type And
  , type IsFalse
  , type They
  , type Describe
  , type Context
  , module ReExport)
  where

import Test.TypeSpec.Core as ReExport
import Test.TypeSpec.Group as ReExport
import Test.TypeSpec.Label as ReExport
import Test.TypeSpec.ShouldBe as ReExport

-- * 'TypeSpec' Aliases

-- | An alias for 'TypeSpec'.
type Expect = TypeSpec

-- | Another alias for 'TypeSpec' (and also 'It')
type Explain does this = TypeSpec (It does this)

-- * 'ShouldBe' aliases

type Is = ShouldBe
type IsTheSameAs = ShouldBe
type TheseAreEqual = ShouldBe

-- * 'ShouldNotBe' aliases

type IsNot = ShouldNotBe
type Isn't = ShouldNotBe
type IsNotTheSameAs = ShouldNotBe
type IsDifferentFrom = ShouldNotBe
type TheseAreNotEqual = ShouldNotBe

-- * 'ShouldBeTrue' aliases

type IsTrue = ShouldBeTrue
type And = ShouldBeTrue
type Therefore = ShouldBeTrue

-- * 'ShouldBeFalse' aliases

type IsFalse = ShouldBeFalse

-- * Labelling Aliases

type They message expectations = It message expectations
type Describe = It
type Context = It
type It's = It
