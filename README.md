
[![Build Status](https://travis-ci.org/sheyll/type-spec.svg?branch=master)](https://travis-ci.org/sheyll/type-spec)

[![Hackage](https://img.shields.io/badge/type-spec-green.svg?style=flat)](http://hackage.haskell.org/package/type-spec)


# A tiny EDSL to write type-level-unit tests.


A small example:

    import Test.TypeSpec

    main :: IO ()
    main = print spec0

    spec :: Expect "Expect something..." (Int `Isn't` Bool)
    spec = Valid

This will output:

    Valid:
         Expect something...
           (✓ Different)

Using the operators from _TypeSpecCrazy_:

    specCrazy ::

        "Higher kinded assertions"
       ###########################

       "ShouldBe accepts types of kind * -> *"
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

           ShouldBe  Maybe Maybe
        -* ShouldBe  []    []
        -* ShouldBe  (->)  (->)

    specCrazy = Valid
    main = print specCrazy

The output:

    Valid:
         Higher kinded assertions
           ShouldBe accepts types of kind * -> *
             • Types are equal
             • Types are equal
             • Types are equal

If you like Lisp, this might be for you:

    type ALot = 1000

    specAliases ::
      (Explain "There are a variety aliases for the basic combinators."
        (Context "Basic Combinators"
          (Describe "Context"
            (It "labels expectations using 'It'"
              (Describe "Describe"
                (It's "an alias for It, just like They"
                  (It's "time for the first assertion"
                    (1000 `Is` ALot))))))))
    specAliases = Valid

    main = print specAliases

This will output:

    Valid:
         There are a variety aliases for the basic combinators.
           Basic Combinators
             Context
               labels expectations using 'It'
                 Describe
                   an alias for It, just like They
                     time for the first assertion
                       (✓ Equal)

The key feature is that the compiler checks the assertions and expectations made
in a 'TypeSpec' and right away rejects invalid types.

When compiling this example:

    specFailing ::
        TypeSpec
            (It "counts the number of elements in a tuple"
                (Count ((),(),()) `ShouldBe` 4))
    specFailing = Valid

    type family Count a :: Nat where
      Count (a,b) = 2
      Count (a,b,c) = 3
      Count (a,b,c,d) = 4

The compiler complains:

    error:
       • counts the number of elements in a tuple
             Expected type: 3
             Actual type:   4
       • In the expression: Valid
         In an equation for ‘specFailing’: specFailing = Valid


After all, with `TypeError` GHC is quite a test-runner.

If you accept to defer type checking and have invalid specs checked during test execution, use
(should-not-typecheck)[https://github.com/CRogers/should-not-typecheck].
