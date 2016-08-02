-- | A result type used in constraints inside 'TypeSpec' to chain computations
-- that may fail with a 'TypeError'.
module Test.TypeSpec.Internal.Result
  (
  -- * Results that with 'TypeErrors'
    type Result
  , type FAILED
  , type OK
  --  * Error propagation
  , type Try
  , type DontTry
  -- * Extending Error Messages
  , type PrependToError
  ) where

import GHC.TypeLits
import Data.Kind
import Test.TypeSpec.Internal.Apply ()
import Test.TypeSpec.Internal.Either ()

-- | When a type level expectation is tested, it might be that compound
-- expectations fail. In order to have a small, precise error message, the type
-- level assertion results are made to have kind 'Result'.
type Result = Either ErrorMessage

-- | A nice alias for 'Left'
type OK     = 'Right
-- | A nice alias for 'Right'
type FAILED = 'Left

-- | Return the result or fail with a 'TypeError'
type family
  Try (e :: Result k) :: k where
  Try (OK     (d :: k)) = d
  Try (FAILED m) = TypeError m

-- | A constraint that is satisfied if the parameter is 'Left'. Fails with a
-- 'TypeError' otherwise.
type family
  DontTry (e :: Result r) :: Constraint where
  DontTry (FAILED e)     = ()
  DontTry (OK okIsNotOk) =
    TypeError ('Text "You specified this wasn't okay: "
               ':$$:
               'Text "    " ':<>: 'ShowType okIsNotOk
               ':$$:
               'Text "... turns out it actually is!")

-- | In case of @'Left' 'ErrorMessage'@ prepend a message to the message, if the
-- parameter was @'Right' x@ just return @'Right' x@.
type family
    PrependToError
      (message :: ErrorMessage)
      (result :: Result a)
      :: Result a
  where
    PrependToError message (OK x) = OK x
    PrependToError message (FAILED otherMessage) =
      FAILED (message ':<>: otherMessage)
