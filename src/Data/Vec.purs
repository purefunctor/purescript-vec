module Data.Vec
  ( Vec
  , empty
  , singleton
  , cons
  , (:)
  , unsafeFromFoldable
  , fromFoldable
  , length
  )
  where

import Prelude

import Data.Foldable (class Foldable, foldMapDefaultL, foldrDefault)
import Data.Foldable as Foldable
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Reflectable (class IsReflectable, reflectType)
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT)
import Type.Prelude (Proxy(..))

newtype Vec :: Int -> Type -> Type
newtype Vec n a = UnsafeMkVec (List a)

instance Eq a => Eq (Vec n a) where
  eq (UnsafeMkVec xs) (UnsafeMkVec ys) = eq xs ys

instance Ord a => Ord (Vec n a) where
  compare (UnsafeMkVec xs) (UnsafeMkVec ys) = compare xs ys

instance (Show a, IsReflectable n Int) => Show (Vec n a) where
  show (UnsafeMkVec xs) = "(fromFoldable (Proxy :: Proxy " <> show (reflectType (Proxy :: Proxy n)) <> ") " <> show xs <> ")"

instance Functor (Vec n) where
  map f (UnsafeMkVec xs) = UnsafeMkVec (map f xs)

instance Foldable (Vec n) where
  foldl f x (UnsafeMkVec xs) = List.foldl f x xs
  foldr f xs x = foldrDefault f xs x
  foldMap f xs = foldMapDefaultL f xs

empty :: forall a. Vec 0 a
empty = UnsafeMkVec Nil

singleton :: forall a. a -> Vec 1 a
singleton a = UnsafeMkVec (Cons a Nil)

cons :: forall a n m. Add n 1 m => a -> Vec n a -> Vec m a
cons x (UnsafeMkVec xs) = UnsafeMkVec (Cons x xs)

infixr 6 cons as :

unsafeFromFoldable
  :: forall a f n
   . Foldable f
  => Compare n (-1) GT
  => Proxy n
  -> f a
  -> Vec n a
unsafeFromFoldable _ xs = UnsafeMkVec (List.fromFoldable xs)

fromFoldable
  :: forall a f n
   . IsReflectable n Int
  => Foldable f
  => Compare n (-1) GT
  => Proxy n
  -> f a
  -> Maybe (Vec n a)
fromFoldable n xs
  | Foldable.length xs == reflectType n =
      Just $ UnsafeMkVec (List.fromFoldable xs)
  | otherwise =
      Nothing

length :: forall a n. IsReflectable n Int => Vec n a -> Int
length _ = reflectType (Proxy :: _ n)
