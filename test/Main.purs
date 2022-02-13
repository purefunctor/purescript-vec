module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Vec (Vec, cons, empty, fromFoldable, length, singleton, (:))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Data.Vec" do
    describe "empty" do
      it "creates an empty vector of size 0" do
        length empty `shouldEqual` 0
    describe "singleton" do
      it "creates a vector of size 1" do
        length (singleton 0) `shouldEqual` 1
    describe "cons" do
      it "increases the vector's size" do
        length (cons 0 empty) `shouldEqual` 1
    describe "fromFoldable" do
      it "creates a vector with a correctly-sized foldable" do
        fromFoldable (Proxy :: _ 2) [1, 2] `shouldEqual` Just (1 : 2 : empty)
      it "fails to create a vector with an incorrectly-sized foldable" do
        fromFoldable (Proxy :: _ 2) [] `shouldEqual` (Nothing :: Maybe (Vec 2 Int))
