module MutableQueue where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)

foreign import data MQueue :: Type -> Type

foreign import size :: forall a. MQueue a -> Effect Int

foreign import newQueue :: forall a. Effect (MQueue a)

foreign import enqueue :: forall a. MQueue a -> a -> Effect Unit

foreign import enqueueAll :: forall a. MQueue a -> Array a -> Effect Unit

foreign import dequeue :: forall a. MQueue a -> Effect (Maybe a)

foreign import toArray :: forall a. MQueue a -> Effect (Array a)
