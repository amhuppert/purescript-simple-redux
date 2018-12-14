module Redux
       ( Reducer(..)
       , ReduxStore
       , subscribe
       , dispatch
       , createStore
       , createStore'
       , getState
       ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import MutableQueue (MQueue)
import MutableQueue as MQ

type Reducer state action = state -> action -> state

newtype ReduxStore state action =
  ReduxStore { reducer :: Reducer state action
             , state :: Ref state
             , subscribers :: Ref (Map Int (state -> Effect Unit))
             , nextSubId :: Ref Int
             , dispatchQueue :: MQueue (Effect Unit)
             , preDispatchHook :: state -> action -> Effect Unit
             }

subscribe :: forall s a. ReduxStore s a -> (s -> Effect Unit) -> Effect (Effect Unit)
subscribe (ReduxStore store) callback = do
  subscriberMap <- Ref.read store.subscribers
  subId <- Ref.read store.nextSubId
  let subscriberMap' = Map.insert subId callback subscriberMap
  Ref.write subscriberMap' store.subscribers
  Ref.write (subId+1) store.nextSubId
  let unsubscribe = Ref.modify_ (Map.delete subId) store.subscribers
  pure unsubscribe

dispatch :: forall s a. ReduxStore s a -> a -> Effect Unit
dispatch (ReduxStore store) action = do
  priorState <- Ref.read store.state
  store.preDispatchHook priorState action
  newState <- Ref.modify (flip store.reducer action) store.state
  subscribers <- Ref.read store.subscribers
  let callbacks = Array.fromFoldable $
                  map (\callback -> callback newState) (Map.values subscribers)
  MQ.enqueueAll store.dispatchQueue callbacks
  tailRecM processQueueStep unit

  where
    processQueueStep :: Unit -> Effect (Step Unit Unit)
    processQueueStep _ = do
      maybeNext <- MQ.dequeue store.dispatchQueue
      case maybeNext of
        Just notify -> notify *> loop
        Nothing -> done
    loop = pure (Loop unit)
    done = pure (Done unit)

createStore' :: forall s a. (s -> a -> Effect Unit) -> Reducer s a -> s -> Effect (ReduxStore s a)
createStore' preDispatchHook reducer initialState = do
  stateRef <- Ref.new initialState
  subscribers <- Ref.new Map.empty
  dispatchQueue <- MQ.newQueue
  nextSubId <- Ref.new 0
  pure $
    ReduxStore
    { reducer
    , state: stateRef
    , subscribers
    , nextSubId
    , dispatchQueue
    , preDispatchHook
    }

createStore :: forall s a. Reducer s a -> s -> Effect (ReduxStore s a)
createStore = createStore' (\_ _ -> pure unit)

getState :: forall s a. ReduxStore s a -> Effect s
getState (ReduxStore store) = Ref.read store.state
