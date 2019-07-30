-- | Simple Redux-like state management.
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

import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Reducer state action = state -> action -> state

newtype ReduxStore state action =
  ReduxStore { reducer :: Reducer state action
             , state :: Ref state
             , subscribers :: Ref (Map Int (Effect Unit))
             , nextSubId :: Ref Int
             , preDispatchHook :: state -> action -> Effect Unit
             }

subscribe :: forall s a. ReduxStore s a -> Effect Unit -> Effect (Effect Unit)
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
  Ref.modify_ (flip store.reducer action) store.state
  subscribers <- Ref.read store.subscribers
  notifySubscribers subscribers
  where
    notifySubscribers subs = traverse_ identity $ Map.values subs

-- | Create a new ReduxStore by specifying the reducer function
-- | and an initial state
-- |
-- | The first argument is an effectful action that is run prior to every dispatch.
-- | It can be used, for example, to log every action.
createStore' :: forall s a. (s -> a -> Effect Unit) -> Reducer s a -> s -> Effect (ReduxStore s a)
createStore' preDispatchHook reducer initialState = do
  stateRef <- Ref.new initialState
  subscribers <- Ref.new Map.empty
  nextSubId <- Ref.new 0
  pure $
    ReduxStore
    { reducer
    , state: stateRef
    , subscribers
    , nextSubId
    , preDispatchHook
    }

createStore :: forall s a. Reducer s a -> s -> Effect (ReduxStore s a)
createStore = createStore' (\_ _ -> pure unit)

getState :: forall s a. ReduxStore s a -> Effect s
getState (ReduxStore store) = Ref.read store.state
