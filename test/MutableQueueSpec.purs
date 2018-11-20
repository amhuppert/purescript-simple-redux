module MutableQueueSpec where

import Prelude

import Control.Monad.Gen (sized)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Compactable (compact)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (intercalateMap)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import MutableQueue (MQueue)
import MutableQueue as MQ
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary, quickCheck, quickCheckGen', (===))
import Test.QuickCheck.Gen (oneOf, resize, vectorOf)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  describe "MutableQueue" do
    it "size (enqueueAll emptyQueue arr) == length arr" do
      liftEffect $ quickCheck \(BiggerArray items :: BiggerArray Int) ->
        let numItems = Array.length items
            queueSize = unsafePerformEffect do
              queue <- MQ.newQueue
              MQ.enqueueAll queue items
              MQ.size queue
         in queueSize === numItems
    it "toArray (enqueueAll emptyQueue arr) == arr" do
      liftEffect $ quickCheck \(BiggerArray items :: BiggerArray Int) ->
        let result = unsafePerformEffect do
              queue <- MQ.newQueue
              MQ.enqueueAll queue items
              MQ.toArray queue
         in items === result
    it "An enqueue operation increases size by one" do
      liftEffect $ quickCheck \(BiggerArray items :: BiggerArray Int) additionalItem ->
        let {preEnqueueSize, postEnqueueSize} = unsafePerformEffect do
              queue <- MQ.newQueue
              MQ.enqueueAll queue items
              preEnqueueSize <- MQ.size queue
              MQ.enqueue queue additionalItem
              postEnqueueSize <- MQ.size queue
              pure { preEnqueueSize, postEnqueueSize }
        in preEnqueueSize + 1 === postEnqueueSize
    it "A series of enqueues followed by the same number of dequeues gives you back what you put in, in the same order" do
      liftEffect $ quickCheck \(BiggerArray inputItems :: BiggerArray Int) ->
        let numItems = Array.length inputItems
            outputItems = unsafePerformEffect do
              queue <- MQ.newQueue
              for_ inputItems $ MQ.enqueue queue
              outputItems' <- compact <$> replicateA numItems do
                MQ.dequeue queue
              pure outputItems'
        in inputItems == outputItems
    it "Behavior of MutableQueue matches that of a Data.Array implementation" do
      liftEffect $ quickCheckGen' 100 do
        BiggerArray ops <- sized \n -> do
          resize (n / 10) arbitrary
        let result = unsafePerformEffect do
              queue <- MQ.newQueue
              let initial = { queue, model: [], remainingOps: ops, processedOps: [] }
              tailRecM modelTestStep initial
        pure result
  where
    modelTestStep :: Accum -> Effect (Step Accum Result)
    modelTestStep accum =
      case Array.uncons accum.remainingOps of
        Just { head, tail } -> do
          let processedOps = accum.processedOps <> [head]
          case head of
            Enqueue n -> do
              MQ.enqueue accum.queue n
              let model' = accum.model <> [n]
              result <- checkPostConditions accum.queue model'
              if isSuccess result
                 then pure $ Loop $ accum { model = model'
                                          , remainingOps = tail
                                          , processedOps = processedOps
                                          }
                 else invalidState accum.queue model' processedOps
            Dequeue -> do
              size <- MQ.size accum.queue
              maybeItem <- MQ.dequeue accum.queue
              let modelResult = Array.head accum.model
                  maybeModel = Array.tail accum.model
                  result1 = maybeItem === modelResult
              result2 <-
                case maybeModel of
                  Just model' -> checkPostConditions accum.queue model'
                  Nothing -> pure $ size === 0
              if isSuccess $ combineResult result1 result2
                then pure $ Loop $ accum { model = fromMaybe [] maybeModel
                                         , remainingOps = tail
                                         , processedOps = processedOps
                                         }
                else invalidState accum.queue (fromMaybe [] maybeModel) processedOps
            EnqueueAll ns -> do
              let model' = accum.model <> ns
              MQ.enqueueAll accum.queue ns
              queueArr <- MQ.toArray accum.queue
              result <- checkPostConditions accum.queue model'
              if isSuccess result
                 then pure $ Loop $ accum { model = model'
                                          , remainingOps = tail
                                          , processedOps = processedOps
                                          }
                else invalidState accum.queue model' processedOps
            GetSize -> do
              let modelSize = Array.length accum.model
              size <- MQ.size accum.queue
              result <- checkPostConditions accum.queue accum.model
              if isSuccess result
                 then pure $ Loop $ accum { remainingOps = tail
                                          , processedOps = processedOps
                                          }
                 else invalidState accum.queue accum.model processedOps
        Nothing -> pure $ Done Success

invalidState :: MQueue Int -> Array Int -> Array Q_Op -> Effect (Step Accum Result)
invalidState queue model processedOps = do
  qarray <- MQ.toArray queue
  let opsMsg = maybe "" (intercalateMap "\n" show) (NEA.fromArray processedOps)
      msg = "MQueue state " <> show qarray <> "\n /= \nModel " <> show model <> "\n\nOperations:\n" <> opsMsg
  pure $ Done $ Failed msg


isSuccess :: Result -> Boolean
isSuccess Success = true
isSuccess _ = false

-- applyOpToModel :: Array Int -> Q_Op -> Array Int
-- applyOpToModel model = case _ of
--   Enqueue n -> model <> [n]
--   Dequeue n

checkPostConditions :: MQueue Int -> Array Int -> Effect Result
checkPostConditions queue model = do
  qSize <- MQ.size queue
  qArray <- MQ.toArray queue
  pure $ combineResult
    (qSize === Array.length model)
    (qArray === model)

combineResult :: Result -> Result -> Result
combineResult Success b = b
combineResult a Success = a
combineResult (Failed firstMsg) (Failed sndMsg) =
  let msg = firstMsg <> "\n" <> sndMsg
   in Failed msg

data Q_Op = Enqueue Int
          | Dequeue
          | EnqueueAll (Array Int)
          | GetSize

newtype BiggerArray a = BiggerArray (Array a)

instance arbBiggerArray :: Arbitrary a => Arbitrary (BiggerArray a) where
  arbitrary = sized \n -> do
    let max = 50 * n
    BiggerArray <$> vectorOf max arbitrary

instance showOp :: Show Q_Op where
  show = genericShow

derive instance genericOp :: Generic Q_Op _

type Accum = { remainingOps :: Array Q_Op
             , processedOps :: Array Q_Op
             , model :: Array Int
             , queue :: MQueue Int
             }

instance arbQ_Op :: Arbitrary Q_Op where
  arbitrary =
      oneOf $ NonEmpty enqueueGen [ dequeueGen, enqueueAllGen, getSizeGen ]
    where
      enqueueGen = Enqueue <$> arbitrary
      dequeueGen = pure Dequeue
      enqueueAllGen = do
        BiggerArray arr <- arbitrary
        pure $ EnqueueAll arr
      getSizeGen = pure GetSize
