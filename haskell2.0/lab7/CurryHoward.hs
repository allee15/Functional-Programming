import Prelude (undefined)

data False                                        -- empty type

data True = True                                  -- unit type

data And a b = And { proj1 :: a, proj2 :: b }     -- product

data Or a b                                       -- sum
  = Left a
  | Right b

type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)

-- Natural deduction introduction and elimination rules

trueIntro :: True                                   -- true introduction
trueIntro = True

falseElim :: False -> b                             -- false elimination
falseElim x = case x of

implIntro :: (a -> b) -> (a -> b)                   -- implication introduction
implIntro f = f

implElim :: (a -> b) -> a -> b                      -- implication elimination
implElim f = f

andIntro :: a -> b -> And a b                       -- and introduction
andIntro = And

andElimL :: And a b -> a                            -- and elimination 1
andElimL = proj1

andElimR :: And a b -> b                            -- and elimination 2
andElimR = proj2

orIntroL :: a -> Or a b                             -- or introduction 1
orIntroL = Left

orIntroR :: b -> Or a b                             -- or introduction 2
orIntroR = Right

orElim :: Or a b -> (a -> c) -> (b -> c) -> c       -- or elimination
orElim or fac fbc = case or of
	Left a -> fac a
	Right b -> fbc b

notElim :: Not p -> p -> c                          -- not elimination 
notElim np p = falseElim(np p)

notIntro :: (forall p. a -> p) -> Not a             -- not introduction
notIntro f = f

iffIntro :: (a -> b) -> (b -> a) -> Iff a b         -- iff introduction
iffIntro fab fba = andIntro fab fba

iffElimL :: Iff a b -> a -> b                       -- iff elimination 1
iffElimL iff a = (andElimL iff) a

iffElimR :: Iff a b -> b -> a                       -- iff elimination 1
iffElimR iff b = (andElimR iff) b

-- Hilbert-style axiomatization for intuitionistic propositional logic

ax1 :: a -> b -> a
ax1 = implIntro(\a -> implIntro (\b -> a))

ax2 :: (a -> b) -> (a -> (b -> c)) -> a -> c
ax2 = implIntro (\f -> implIntro (\g -> implIntro (\a -> implElim (implElim g a) (implElim f a)))) 

ax3 :: a -> b -> And a b
ax3 = implIntro (\a -> implIntro (\b -> andIntro a b)) 

ax4 :: And a b -> a
ax4 = implIntro(\ab -> andElimL ab)

ax5 :: And a b -> b
ax5 ab = andElimR ab

ax6 :: a -> Or a b
ax6 a = orIntroL a

ax7 :: b -> Or a b
ax7 b = orIntroR b

ax8 :: (a -> c) -> (b -> c) -> Or a b -> c
ax8 ac bc or = orElim or ac bc

ax9 :: (a -> b) -> (a -> Not b) -> Not a
ax9 ab anb a = notElim (anb a) (ab a)

ax10 :: Not a -> a -> b
ax10 na a = falseElim (na a)


deMorgan1 :: And (Not p) (Not q) -> Not (Or p q)
deMorgan1 nand = notIntro (\or -> case or of
  Left p -> notElim (andElimL nand) p
  Right q -> notElim (andElimR nand) q)

deMorgan2 :: Not (Or p q) -> And (Not p) (Not q)
deMorgan2 nor = andIntro 
  (notIntro (\p -> notElim nor (orIntroL p)))
  (notIntro (\q -> notElim nor (orIntroR q)))

deMorgan3 :: Or (Not p) (Not q) -> Not (And p q)
deMorgan3 nor = notIntro (\and -> case and of
  And p q -> case (orElim nor (\np -> np p) (\nq -> nq q)) of)


type DeMorgan4 = forall p q . Not (And p q) -> Or (Not p) (Not q)

-- Classical axioms

type ExcludedMiddle = forall a. Or a (Not a)
type DoubleNegation = forall a. Not (Not a) -> a
type PeirceLaw = forall p q. ((p -> q) -> p) -> p

excludedMiddleImplDoubleNeg :: ExcludedMiddle -> DoubleNegation
excludedMiddleImplDoubleNeg em dn = dn (notIntro (\na -> orElim (em na) trueIntro notElim))

doubleNegImplExcludedMiddle :: DoubleNegation -> ExcludedMiddle
doubleNegImplExcludedMiddle dn = orIntroR (notIntro (\a -> notElim (notIntro (\na -> dn (notIntro (\naa -> notElim na a)))) a))

classicDeMorgan4 :: ExcludedMiddle -> DeMorgan4
classicDeMorgan4 em = notIntro (\and -> case and of
  And p q -> case (em p) of
    Left p' -> orIntroL (notIntro (\np -> notElim (notIntro (\pnp -> pnp (andIntro p' q))) np))
    Right np -> orIntroR (notIntro (\nq -> notElim (notIntro (\qnp -> qnp (andIntro p q))) nq)))
