-- | Useful abstractions for type level programming using. This reimplements
-- parts of the singletons library, which is just too heavy of a dependency to
-- carry around, when only three small types are used of it.
module Test.TypeSpec.Internal.Apply
    ( type (>>=)
    , type (>>)
    , type (<*>)
    , type (<$>$$)
    , type (<$>$)
    , type (<$>)
    , type TyCon1
    , type TyCon2
    , type Apply
    , TyFun, type TyFunData
    , type (~>)
    , type Cons''
    , type Cons'
    , type Pair''
    , type Pair'
    , type Const
    , type Const'
    , type Const''
    , Flip'
    , Flip
    , Flip_
    , type Flip__
    , Compose''
    , Compose'
    , Compose
    , type Compose_
    )
    where

import Data.Kind

-- | Bind to actions.
type family
 (>>=) (ma :: monad a)
       (f :: TyFunData (a :: Type) ((monad b) :: Type))
       :: monad b

-- | Execute one action and then the next, ignore the result of the first.
type (>>) ma mb = ma >>= Const' mb

-- | Execute an action that returns a function than map function over the result
-- of the next action.
type family
  (f :: m (a ~> b)) <*> (ma :: m a) :: m b where
  mf <*> mx = mf >>= Apply (Flip (<$>$$)) mx

-- | Tuple construction
data Pair'' :: a ~> b ~> (a, b)
data Pair'  :: a -> b ~> (a, b)
type instance Apply Pair'' x = Pair' x
type instance Apply (Pair' x) y = '(x, y)

-- | List construction
data Cons'' :: a ~> [a] ~> [a]
data Cons' :: a -> [a] ~> [a]
type instance Apply Cons'' x = Cons' x
type instance Apply (Cons' x) xs = x ': xs

-- | Convert data types to Partially applicable type functions
data TyCon1 :: (a -> b) -> a ~> b
data TyCon2 :: (a -> b -> c) -> a ~> b ~> c
type instance Apply (TyCon1 f) x = f x
type instance Apply (TyCon2 f) x = (TyCon1 (f x))

-- | Execute an action and map a pure function over the result.
data (<$>$$) :: (a ~> b) ~> m a ~> m b
data (<$>$) :: (a ~> b) -> m a ~> m b
type instance Apply (<$>$$) f = (<$>$) f
type instance Apply ((<$>$) f) x = f <$> x
type family
  (f :: (a ~> b)) <$> (ma :: m a) :: m b

-- * Flip Type Functions

data Flip' :: (a ~> b ~> c) ~> b ~> a ~> c
data Flip :: (a ~> b ~> c) -> b ~> a ~> c
data Flip_ :: (a ~> b ~> c) -> b -> a ~> c
type instance Apply Flip' f = Flip f
type instance Apply (Flip f) y = Flip_ f y
type instance Apply (Flip_ f y) x = Flip__ f y x
type family
  Flip__ (f :: (a ~> b ~> c)) (y :: b) (x :: a) :: c where
  Flip__ f y x = Apply (Apply f x) y

-- * Type Function composition

data Compose'' :: (b ~> c) ~> (a ~> b) ~> (a ~> c)
data Compose' :: (b ~> c) -> (a ~> b) ~> (a ~> c)
data Compose :: (b ~> c) -> (a ~> b) -> (a ~> c)
type instance Apply Compose'' f = Compose' f
type instance Apply (Compose' f) g = (Compose f g)
type instance Apply (Compose f g) x = Compose_ f g x
type family
  Compose_ (f :: b ~> c) (g :: a ~> b) (x :: a) :: c where
  Compose_ f g x = Apply f (Apply g x)


-- * Type-Level 'const'

type family Const (a :: t) (b :: t') :: t where Const a b = a
data Const' :: a -> (TyFunData b a)
data Const'' :: TyFunData a (TyFunData b a)
type instance Apply Const'' a = Const' a
type instance Apply (Const' a) b = Const a b

-- * Defunctionalization

data TyFun :: Type -> Type -> Type
type TyFunData a b = TyFun a b -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: a ~> b) (x :: a) :: b
