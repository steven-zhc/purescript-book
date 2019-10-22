module Exercises where

import Prelude

-- chapter 6
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

-- 4 