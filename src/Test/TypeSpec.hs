{-|
Module      : Test.TypeSpec
Description : Type-Level eDSL for Type-Unit-Tests
Copyright   : (c) Sven Heyll, 2016
License     : BSD-3
Maintainer  : sven.heyll@gmail.com
Stability   : experimental

A tiny EDSL to write type-level-unit tests.

A simple example:

> specHelloWorld :: Expect (Int `Isn't` Bool)
> specHelloWorld = Valid

We can also /expect/ a bit more using lists and tuples:

> specGrouped
>   :: Expect '[ Int  `Isn't` Bool
>              , Int  `Is`    Int
>              , Bool `Is`    Bool  `ButNot`  String
>              ]
> specGrouped = Valid

The expectations are /executed/ by the compiler when solving the constraints of
'TypeSpec's constructors.

A 'TypeSpec' also has a 'Show' instance, which can be used in real unit tests
to print the expectations.

 This module contains mainly re-exports of.

 * "Test.TypeSpec.Core"

 * "Test.TypeSpec.Group"

 * "Test.TypeSpec.Label"

 * "Test.TypeSpec.ShouldBe"

-}
module Test.TypeSpec
  (
  -- * 'TypeSpec' Aliases

  type Expect,
  type Explain,

  -- * 'ShouldBe' aliases

  type Is,
  type IsTheSameAs,
  type TheseAreEqual,

  -- * 'ShouldNotBe' aliases

  type IsNot,
  type Isn't,
  type IsNotTheSameAs,
  type IsDifferentFrom,
  type TheseAreNotEqual,

  -- * 'ShouldBeTrue' aliases

  type IsTrue,
  type And,
  type Therefore,
  type That,

  -- * 'ShouldBeFalse' aliases

  type IsFalse,
  type Not,

  -- * Labelling Aliases

  type They,
  type Describe,
  type Context,
  type It's,

  -- * Reexports

  module Test.TypeSpec.Core,
  module Test.TypeSpec.Group,
  module Test.TypeSpec.Label,
  module Test.TypeSpec.ShouldBe

  )
  where

import Test.TypeSpec.Core
import Test.TypeSpec.Group
import Test.TypeSpec.Label
import Test.TypeSpec.ShouldBe

-- * 'TypeSpec' Aliases

type Expect = TypeSpec
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
type That = ShouldBeTrue

-- * 'ShouldBeFalse' aliases

type IsFalse = ShouldBeFalse
type Not = ShouldBeTrue

-- * Labelling Aliases

type They message expectations = It message expectations
type Describe = It
type Context = It
type It's = It
