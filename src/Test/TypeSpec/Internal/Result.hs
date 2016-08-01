-- | Result type used in constraints inside 'TypeSpec' to propagate type errors.
module Test.TypeSpec.Internal.Result
  ( type Result
  , type Try
  , type DontTry
  , type FAILED
  , type OK
  , type PrependToError
  ) where

import GHC.TypeLits
import Data.Kind
import Test.TypeSpec.Internal.Apply ()
import Test.TypeSpec.Internal.Either ()

--  * Type error propagation

-- | When a type level expectation is tested, it might be that compound
-- expectations fail. In order to have a small, precise error message, the type
-- level assertion results are made to have kind 'Result'.
type Result = Either ErrorMessage

type family
  Try (e :: Result k) :: k where
  Try (OK     (d :: k)) = d
  Try (FAILED m) = TypeError m

type family
  DontTry (e :: Result r) :: Constraint where
  DontTry (FAILED e)     = ()
  DontTry (OK okIsNotOk) =
    TypeError ('Text "You specified this wasn't okay: "
               ':$$:
               'Text "    " ':<>: 'ShowType okIsNotOk
               ':$$:
               'Text "... turns out it actually is!")

-- | A nice name than 'Left'
type OK     = 'Right
type FAILED = 'Left

type family
    PrependToError
      (message :: ErrorMessage)
      (result :: Result a)
      :: Result a
  where
    PrependToError message (OK x) = OK x
    PrependToError message (FAILED otherMessage) =
      FAILED (message ':<>: otherMessage)
