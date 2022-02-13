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

import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Reflectable (class IsReflectable, reflectType)
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT)
import Type.Prelude (Proxy(..))

data Vec :: Int -> Type -> Type
data Vec n a = UnsafeMkVec (Proxy n) (List a)

empty :: forall a. Vec 0 a
empty = UnsafeMkVec (Proxy :: _ 0) Nil

singleton :: forall a. a -> Vec 1 a
singleton a = UnsafeMkVec (Proxy :: _ 1) (Cons a Nil)

cons :: forall a n m. Add n 1 m => a -> Vec n a -> Vec m a
cons x (UnsafeMkVec _ xs) = UnsafeMkVec (Proxy :: _ m) (Cons x xs)

infixr 6 cons as :

unsafeFromFoldable
  :: forall a f n
   . Foldable f
  => Compare n (-1) GT
  => Proxy n
  -> f a
  -> Vec n a
unsafeFromFoldable n xs = UnsafeMkVec n (List.fromFoldable xs)

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
      Just $ UnsafeMkVec n (List.fromFoldable xs)
  | otherwise =
      Nothing

length :: forall a n. IsReflectable n Int => Vec n a -> Int
length _ = reflectType (Proxy :: _ n)