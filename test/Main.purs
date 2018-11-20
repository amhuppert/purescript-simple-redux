module Test.Main where

import Prelude

import Data.Enum (enumFromTo)
import Data.Foldable (find, foldl, for_)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import MutableQueueSpec as MutableQueueSpec
import Redux as Redux
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary, quickCheck, quickCheckGen')
import Test.QuickCheck.Gen (Gen, arrayOf, elements, frequency, oneOf)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  MutableQueueSpec.spec
  describe "Reducer" do
    it "Dispatching multiple actions is the same as folding over the same actions with the reducer" do
      liftEffect $ quickCheck \(initialState :: Int) (actions :: Array Int) ->
        let reducer = (-)
            stateAfterDispatches = unsafePerformEffect do
              store <- Redux.createStore reducer initialState
              for_ actions (Redux.dispatch store)
              Redux.getState store
            stateAfterFold = foldl reducer initialState actions
         in stateAfterDispatches == stateAfterFold
  describe "Subscribers" do
    it "All subscribers are notified about each iteration of the state in the same order" do
      liftEffect $ quickCheckGen' 1000 do
        (initialState :: String) <- genStr
        (actions :: Array String) <- arrayOf genStr
        (subscribers :: Array SubCallbackSpec) <- arbitrary
        let reducer = (<>)
            {states,subscriberSeenStates} = unsafePerformEffect do
              statesRef <- Ref.new []
              store <- Redux.createStore reducer initialState
              let dispatch = \action -> do
                    currState <- Redux.getState store
                    let nextState = reducer currState action
                    Ref.modify_ (_ <> [nextState]) statesRef
                    Redux.dispatch store action
              subscriberSeenStateRefs :: Array (Ref (Array String)) <- for subscribers \subSpec -> do
                seenStatesRef <- Ref.new []
                remainingActionsRef <- Ref.new Nil
                case subSpec of
                  Dispatch actionsToDispatch -> Ref.write (List.fromFoldable actionsToDispatch) remainingActionsRef
                  _ -> pure unit
                void $ Redux.subscribe store \s -> do
                  Ref.modify_ (_ <> [s]) seenStatesRef
                  remainingActions <- Ref.read remainingActionsRef
                  case List.uncons remainingActions of
                    Just {head,tail} -> do
                      Ref.write tail remainingActionsRef
                      dispatch head
                    Nothing -> pure unit
                pure seenStatesRef
              for_ actions dispatch
              states <- Ref.read statesRef
              subscriberSeenStates <- for subscriberSeenStateRefs Ref.read
              pure {states, subscriberSeenStates}
        case find (_ /= states) subscriberSeenStates of
          Just _ -> do
            let msg = show states <> " /= " <> show subscriberSeenStates
            pure $ Failed msg
          Nothing -> pure Success

-- Describes how a subscriber callback behaves.
-- It either does nothing, or dispatches an action.
data SubCallbackSpec = NoOp | Dispatch (Array String)

instance arbSubCallbackSpec :: Arbitrary SubCallbackSpec where
  arbitrary = frequency $
              NonEmpty
              (Tuple 10.0 (pure NoOp))
              (List.fromFoldable [ Tuple 4.0 (Dispatch <$> (arrayOf genStr))])

genStr :: Gen String
genStr = fromCharArray <$> arrayOf genLetter

genLetter :: Gen Char
genLetter = do
  let lower = elements $ NonEmpty 'a' (enumFromTo 'b' 'z')
      upper = elements $ NonEmpty 'A' (enumFromTo 'B' 'Z')
  oneOf $ NonEmpty lower [upper]
