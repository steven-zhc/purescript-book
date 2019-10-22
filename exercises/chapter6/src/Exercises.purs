module Exercises where

import Prelude

-- chapter 6

-- ==============================================================
-- exercies 6.4
-- 1 Define Show and Eq instances for Complex.
newtype Complex = Complex
    { 
        real :: Number, 
        imaginary :: Number
    }

instance showComplex :: Show Complex  where
    show (Complex {real, imaginary}) = "real: " <> show real <> ", imaginary: " <> show imaginary <> "."

instance eqComplex :: Eq Complex where
    eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

newtype HashCode = HashCode Int

instance showHashCode :: Show HashCode where
    show (HashCode h) = "(HashCode " <> show h <> ")"

-- ==============================================================
-- exercies 6.7
-- 1
data NonEmpty a = NonEmpty a (Array a)

-- Write an Eq instance for the type NonEmpty a
instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
    eq (NonEmpty x []) (NonEmpty y []) = eq x y
    eq (NonEmpty x []) (NonEmpty y ys) = eq [x] ys
    eq (NonEmpty _ xs) (NonEmpty y []) = eq xs [y]
    eq (NonEmpty x xs) (NonEmpty y ys) = eq xs ys

-- 
instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
    show (NonEmpty x xs) = show ([x] <> xs)

-- 2  Write a Semigroup instance for NonEmpty a by reusing the Semigroup instance for Array.
instance appendNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a) where
    append (NonEmpty x []) (NonEmpty y []) = NonEmpty x []
    append (NonEmpty x []) (NonEmpty y ys) = NonEmpty x ys
    append (NonEmpty x xs) (NonEmpty y []) = NonEmpty x (xs <> y)
    append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> ys)

-- 3 Write a Functor instance for NonEmpty.
instance mapNonEmpty :: Functor (NonEmpty) where
    map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

-- 4 Given any type a with an instance of Ord, we can add a new “infinite” value which is greater than any other value:
data Extended a = Finite a | Infinite

-- Write an Ord instance for Extended a which reuses the Ord instance for a.
instance compareExtended :: (Ord a) => Ord (Extended a) where
    compare Infinite Infinite = EQ
    compare Infinite (Finite a) = GT
    compare (Finite a) Infinite = LT
    compare (Finite x) (Finite y) = compare x y

-- 5 (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable instance for arrays
instance foldableNonEmpty :: Foldable NonEmpty where
    foldl f acc (NonEmpty x []) = foldl f acc [x]
    foldl f acc (NonEmpty _ xs) = foldl f acc xs

    foldr f acc (NonEmpty x []) = foldr f acc [x]
    foldr f acc (NonEmpty _ xs) = foldr f acc xs

    foldMap f (NonEmpty x []) = foldMap f [x]
    foldMap f (NonEmpty _ xs) = foldMap f xs

-- 6 (Difficult) Given an type constructor f which defines an ordered container (and so has a Foldable instance), 
-- we can create a new container type which includes an extra element at the front:

data OneMore f a = OneMore a (f a)

-- The container OneMore f is also has an ordering, 
-- where the new element comes before any element of f. 
-- Write a Foldable instance for OneMore f:

-- instance foldableOneMore :: Foldable f => Foldable (OneMore f) where