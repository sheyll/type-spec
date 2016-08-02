-- | Some examples for 'Test.TypeSpec'
module Main where

import GHC.TypeLits
import Test.TypeSpecCrazy

main :: IO ()
main = do
  print specHelloWorld
  print specGrouped
  print specAliases
  print specTuple
  print specFamilyInstancesSuperCrazy
  print spec1
  print specCrazy
  print specInvalidCrazy
  print specCrazyMoreNested

-- * TypeSpec Examples

-- | Let's start off simple:
specHelloWorld :: Expect (Int `Isn't` Bool)
specHelloWorld = Valid

-- | We can also expect a bit more using lists and tuples:
specGrouped
  :: Expect '[ Int  `Isn't` Bool
             , Int  `Is`    Int
             , Bool `Is`    Bool  `ButNot`  String
             ]
specGrouped = Valid

specTuple ::
  Explain "Type level tuples can also be used to group tests."
    (They "accept only two elments"
      '( (5 - 4) `Is` 1, (3 + 3) `Is` 6 `ButNot` (3 - 3) ) )
specTuple = Valid

-- * Some nice aliases

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


-- * More complex example

type family Swap a
type instance Swap (a,b) = (b,a)

type family SwapT (a :: (k1, k2)) :: (k2, k1)
type instance SwapT '(a,b) = '(b,a)

type family Fst a where
  Fst (a,b) = a

type family Count a :: Nat where
  Count (a,b) = 2
  Count (a,b,c) = 3
  Count (a,b,c,d) = 4

-- A failing test case example:
-- specFailing ::
--     TypeSpec
--       (It "counts the number of elements in a tuple"
--           (Count ((),(),()) `ShouldBe` 4))
-- specFailing = Valid

specFamilyInstancesSuperCrazy ::

    "Only one title like this"
    ###########################

    "Describe the following expectations"

     --* Swap (Int, Bool) `Isn't` (Int, Int)
      -* Swap (Int, Bool) `Is`    (Bool, Int)
      -* Swap (Int, Bool) `Isn't` Swap (Swap (Int, Bool))
      -* (Int, Bool)      `Is`    Swap (Swap (Int, Bool))
      -*
         ("... now there is maybe a nested block"

           --* Count    (Int,Int,Int,Int) `ShouldBe`      4
            -* SwapT               '(0,1) `ShouldBe` '(1,0)
            -* And   (Count (Int,Int,Int)     <=?         4))

    -/-

    "Here there is another top-level block"
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     [ Count (Int,Int,Int,Int)    `Is`  4
     , SwapT '(0,1)               `Is`  '(1,0)
     , And   (Count (Int,Int,Int)  <=?   4)
     ]


specFamilyInstancesSuperCrazy = Valid

-- * More examples

spec1 ::
  Explain "TypeSpec"
    (It "Allows explanations of types"  (ShouldBe Int Int))
spec1 = Valid

specCrazy ::

    "Higher kinded assertions"
   ###########################

   "ShouldBe accepts types of kind * -> *"
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

       ShouldBe  Maybe Maybe
    -* ShouldBe  []    []
    -* ShouldBe  (->)  (->)

specCrazy = Valid

-- --------------------------------------------------------

type SpecInvalidCrazy =

  "One of the following specs is not OK"
 #######################################

 "This should be ok"
 ~~~~~~~~~~~~~~~~~~~~~

 TheseAreEqual    Bool Bool

 -/-

 "This should also be ok"
 ~~~~~~~~~~~~~~~~~~~~~~~~

 TheseAreNotEqual (Maybe Int) (Maybe Int)

 -/-

 "But this looks bad:"
 ~~~~~~~~~~~~~~~~~~~~~

 TheseAreEqual    Maybe (Either Int)

specInvalidCrazy :: SpecInvalidCrazy
specInvalidCrazy = Invalid


specCrazyMoreNested ::

  "Title"
  ######

    "Top-level "
    ~~~~~~~~~~~~

         "Nested:"
         ~~~~~~~~~
         Int `Is` Int
                            -*-
         Int `Is` Int
                            -*-
         Int `Is` Int

                            -/-

    "Top-level "
    ~~~~~~~~~~~~

         "Nested:"
         ~~~~~~~~~
         Int `Is` Int
                            -*-
             "Nested:"
             ~~~~~~~~~
             Int `Is` Int
                                -*-
             Int `Is` Int

specCrazyMoreNested = Valid
