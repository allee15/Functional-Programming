module NatClass where

import Prelude (Show(..), (<>), Num(fromInteger), undefined)
import qualified GHC.Natural as Natural

import MyPrelude
import BoolClass
import MaybeClass
 
-- | The class of Natural-like types (types having a notion of
-- 'zero', 'succ'essor, and 'iter'ation).
-- Instances should satisfy the following:
--
-- [Zero Iterations] @'iter' f i 'zero' = i@
-- [Successor Iterations]  @'iter' f i ('succ' n) = f ('iter' f i n)@
class NatClass n where
  zero :: n
  succ :: n -> n
  iter :: (a -> a) -> a -> n -> a

instance NatClass Natural.Natural where
  zero = 0
  succ n = n `Natural.plusNatural` 1
  iter f i 0 = i
  iter f i n = f (iter f i (n `Natural.minusNatural` 1))

one :: NatClass n => n
one = succ zero

-- >>> one :: Natural.Natural
-- 1

-- | The isZero function returns 'true' iff its argument is 'zero'.
isZero :: NatClass n => n -> CBool
isZero x = iter _ true x

-- >>> isZero (one :: Natural.Natural)
-- CFalse

-- | Usual natural numbers addition
add :: NatClass n => n -> n -> n
add x y = iter succ x y

-- >>> add one one :: Natural.Natural
-- 2

-- | Usual natural numbers multiplication
mul :: NatClass n => n -> n -> n
mul x y = iter (add x) zero y

-- >>> mul one one :: Natural.Natural
-- 1

-- | Usual natural numbers exponentiation (@exp m n@ is @m ^ n@)
exp :: NatClass n => n -> n -> n
exp x y = iter (mul x) one y

-- >>> exp (add one one) (add one one) :: Natural.Natural
-- 4

-- | Predecessor of a natural number ('nothing' for 'zero')
pred :: NatClass n  => n -> CMaybe n
pred x = iter f nothing x
  where
    f :: CMaybe n -> CMaybe n
    f CNothing = CNothing
    f (CJust y) = CJust (iter succ zero y)

-- >>> pred zero :: CMaybe Natural.Natural
-- CNothing

-- | Difference between natural numbers as a 'MaybeClass' ('nothing' if first is smaller)
sub :: NatClass n  => n -> n -> CMaybe n
sub x y
  | lt x y == CTrue = CNothing
  | otherwise = iter f (CJust x) y
  where
    f :: CMaybe n -> CMaybe n
    f CNothing = CNothing
    f (CJust z) = CJust (pred z)

-- >>> sub (exp (add one one) (add one one)) one :: CMaybe Natural.Natural
-- CJust 3

-- | (Strictly-)Less-Than predicate for natural numbers.
lt :: NatClass n => n -> n -> CBool 
lt x y = iter f CFalse x
  where
    f :: CBool -> CBool
    f CTrue = CTrue
    f CFalse = lt' y
    lt' z = case pred z of
      CNothing -> CFalse
      CJust w -> iter f (CJust CTrue) w

-- >>> lt (one :: Natural.Natural) one
-- CFalse

-- | (Strictly-)Greater-Than predicate for natural numbers.
gt :: NatClass n => n -> n -> CBool 
gt x y = iter f CFalse x
  where
    f :: CBool -> CBool
    f CTrue = CTrue
    f CFalse = gt' y
    gt' z = case pred z of
      CNothing -> CTrue
      CJust w -> iter f (CJust CTrue) w

-- >>> gt (one :: Natural.Natural) one
-- CFalse

-- | Greater-Than-or-Equal-To predicate for natural numbers.
gte :: NatClass n => n -> n -> CBool 
gte x y = case sub x y of
  CNothing -> CFalse
  CJust _ -> CTrue

-- >>> gte (zero :: Natural.Natural) zero
-- CTrue

-- | Less-Than-or-Equal-To predicate for natural numbers.
lte :: NatClass n => n -> n -> CBool 
lte x y = isZero $ sub x y

-- >>> lte (zero :: Natural.Natural) zero
-- CTrue

-- | Equality predicate for natural numbers.
eq :: NatClass n => n -> n -> CBool 
eq x y = andC (lte x y) (lte y x)

-- >>> eq (zero :: Natural.Natural) zero
-- CTrue

-- >>> eq (zero :: Natural.Natural) one
-- CFalse

-- | Returns the greater between its two arguments
max :: NatClass n => n -> n -> n
max x y = if lt x y then y else x

-- >>> max (zero :: Natural.Natural) one
-- 1

newtype CNat = CNat { getCNat :: forall a . (a -> a) -> a -> a }

instance NatClass CNat where
  iter f i n = getCNat n f i
  zero = CNat (\_ i -> i)
  succ n = CNat (\f i -> f (getCNat n f i))

-- | converting between different instances of 'NatClass'
fromNatClass :: (NatClass n, NatClass m) => n -> m
fromNatClass = iter succ zero

-- | 'Show' instance for 'CNat' (via transformation into Haskell Natural)
instance Show CNat where
  show cn = "C" <> show (fromNatClass cn :: Natural.Natural)

-- | to allow recognizing natural numbers as 'CNat'
instance Num CNat where
  fromInteger n = fromNatClass (fromInteger n :: Natural.Natural)

-- >>> 1 :: CNat
-- C1

-- >>> isZero (1 :: CNat)
-- CFalse

-- >>> add 3 5 :: CNat
-- C8

-- >>> mul 2 7 :: CNat
-- C14

-- >>> exp 2 10 :: CNat
-- C1024

-- >>> pred 0 :: CMaybe CNat
-- CNothing

-- >>> sub 5 5 :: CMaybe CNat
-- CJust C0

-- >>> lt 7 (7 :: CNat)
-- CFalse

-- >>> gt (one :: CNat) one
-- CFalse

-- >>> gte (zero :: CNat) zero
-- CTrue

-- >>> lte (zero :: CNat) zero
-- CTrue

-- >>> eq (zero :: CNat) zero
-- CTrue

-- >>> eq (zero :: CNat) one
-- CFalse

-- >>> max (zero :: CNat) one
-- C1
