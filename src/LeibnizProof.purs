module LeibnizProof where

newtype Leibniz a b = Leibniz (forall f. f a -> f b)

infix 4 type Leibniz as ~

coe :: forall f a b. (a ~ b) -> f a -> f b
coe (Leibniz f) = f

refl :: forall a. (a ~ a)
refl = Leibniz (\x -> x)

-- no-cheating `coerce` implementation:

newtype Identity a = Identity a

unIdentity :: forall a. Identity a -> a
unIdentity (Identity a) = a

coerce :: forall a b. (a ~ b) -> a -> b
coerce p a = unIdentity (coe p (Identity a))

-- no-cheating `symm` implementation:

newtype Flip f a b = Flip (f b a)

unFlip :: forall f a b. Flip f a b -> f b a
unFlip (Flip fba) = fba

symm :: forall a b. (a ~ b) -> (b ~ a)
symm p = unFlip (coe p (Flip refl))
