-- | Some examples for 'expr.TypeSpec'
module Main where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.Exts
import GHC.TypeLits
import Test.TypeSpec
import Test.TypeSpecCrazy

main :: IO ()
main = do
  print spec0
  print specGrouped
  print specFamilyInstances
  print spec1
  print spec2
  print spec4

-- * TypeSpec Examples

-- | Let's start of simple:
spec0 :: Expect (Int `Isn't` Bool)
spec0 = Valid

-- | We can also expect a bit more using lists and tuples:
specGrouped
  :: Expect '[ Int  `Isn't` Bool
             , Int  `Is`    Int
             , Bool `Is`    Bool  `ButNot`  String
             ]
specGrouped = Valid

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

specFamilyInstances ::

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


specFamilyInstances = Valid

-- * More examples

spec1 ::
  Explain "TypeSpec"
    (It "Allows explanations of types"  (ShouldBe Int Int))
spec1 = Valid

spec2 ::

   "Poly kinded assertions"
   ########################

   "ShouldBe accepts types of kind * -> *"
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   ShouldBe  Maybe Maybe

spec2 = Valid

spec3 :: Explain "TypeSpec of empty lists" '[]
spec3 = Valid

-- --------------------------------------------------------

type Spec4 =

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

spec4 :: Spec4
spec4 = Invalid

-- --------------------------------------------------------

data TyFun1 :: Type -> Type -> Type
type TyFun2 a b = a -> b

type family SameType (a :: k1) (b :: k2) :: Bool where
  SameType a a = 'True
  SameType a b = 'False
