module Test.Main where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Redux as Redux
import Test.QuickCheck (quickCheck, (==?))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Reducer" do
    it "Dispatching multiple actions is the same as folding over the same actions with the reducer" do
      liftEffect $ quickCheck \(initialState :: Int) (actions :: Array Int) ->
        let reducer = (-)
            stateAfterDispatches = unsafePerformEffect do
              store <- Redux.createStore reducer initialState
              for_ actions (Redux.dispatch store)
              Redux.getState store
            stateAfterFold = foldl reducer initialState actions
         in stateAfterDispatches ==? stateAfterFold
  describe "Subscribers" do
    it "Subscribers are notified" do
      let reducer = Array.snoc
      redux <- liftEffect $ Redux.createStore reducer []
      sub1 <- liftEffect $ Ref.new []
      void $ liftEffect $ Redux.subscribe redux do
        s <- Redux.getState redux
        Ref.write s sub1
      sub2 <- liftEffect $ Ref.new []
      void $ liftEffect $ Redux.subscribe redux do
        s <- Redux.getState redux
        Ref.write s sub2
      liftEffect do
        Redux.dispatch redux 15
        Redux.dispatch redux 10
        Redux.dispatch redux 20
      sub1State <- liftEffect $ Ref.read sub1
      sub2State <- liftEffect $ Ref.read sub2
      sub1State `shouldEqual` [15,10,20]
      sub2State `shouldEqual` [15,10,20]
