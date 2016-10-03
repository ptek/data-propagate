-- | Data.Propagate
--
-- The module provides basic functionality for the propagators. It
-- tries to implement the work on Propagation Networks. We say that it
-- tries to implement it because the implementation may not be
-- complete. Never the less, we are open to discussion or new insights
-- regarding the work
--
module Data.Propagate (
  -- * Data Declarations
  PtEvent
 ,PtQueue
  -- * Placing and retreiving the events onto the queue
 ,empty
 ,mkEvent
 ,fromList
 ,toList
 ,leftMerge
 ,propagate
 ,message
  -- * Working with the Queue
 ,filter
 ,union
 ,accumulate
 ,delay
 ,delayWith
 ) where

import qualified Data.Foldable         as Foldable
import           Data.Function
import           Data.Sequence         (Seq, (><), (|>))
import qualified Data.Sequence         as Seq
import           Data.Time.Clock.POSIX
import           Prelude               hiding (filter, map)

-- | The Queue holding the events
newtype PtQueue a = PtQueue (Seq (PtEvent a)) deriving (Eq,Show)

-- | The Event to be propagated
data PtEvent a = PtEvent POSIXTime a deriving (Eq,Show)


instance Functor PtQueue where
  -- ^ Useful for changing of messages in the queue. Provides fmap
  fmap f (PtQueue s) = PtQueue (fmap (fmap f) s)

instance Monoid (PtQueue a) where
  mempty = empty
  mappend = union

instance Functor PtEvent where
  -- ^ Useful for changing of messages in the queue. Provides fmap
  fmap f (PtEvent t a) = PtEvent t (f a)


-- | Empty queue.
--
-- > propagate (mkEvent now "New Sample") empty
--
empty :: PtQueue a
empty = PtQueue mempty

-- | Place an event onto a queue.
--
-- > propagate (mkEvent now "Another Sample") q
--
propagate :: () => PtEvent a -> PtQueue a -> PtQueue a
propagate e (PtQueue s) = PtQueue (_sortEvents (s |> e))

fromList :: [PtEvent a] -> PtQueue a
fromList = PtQueue . _sortEvents . Seq.fromList

toList :: PtQueue a -> [PtEvent a]
toList (PtQueue s) = Foldable.toList s

mkEvent :: POSIXTime -> a -> PtEvent a
mkEvent t a = PtEvent t a

message :: PtEvent a -> a
message (PtEvent _ m) = m

_timestamp :: PtEvent a -> POSIXTime
_timestamp (PtEvent t _) = t

-- | Union of two queues of the same type.
--
-- NOTE: two events happening at the same time will be added in the
-- order of arguments. Union is not a commutative
-- operation. This means that __@ xs `union` ys /= ys `union` xs @__
--
-- The reason for this is the fact that there is no way to figure out
-- how to sort the messages unless they have an Ord instance. The
-- latter adds too much strictness
--
union :: () => PtQueue a -> PtQueue a -> PtQueue a
union (PtQueue s1) (PtQueue s2) = PtQueue (_sortEvents (s1 >< s2))

leftMerge :: PtQueue a -> PtQueue b -> PtQueue (a,PtQueue b)
leftMerge (PtQueue xs) ys = PtQueue (fmap (adjustedTo ys) xs)
  where
  adjustedTo :: PtQueue z -> PtEvent x -> PtEvent (x,PtQueue z)
  adjustedTo zs (PtEvent t x) = PtEvent t (x, zs `_until` t)

filter :: (a -> Bool) -> PtQueue a -> PtQueue a
filter p (PtQueue s) = PtQueue (Seq.filter (p . message) s)

-- | A simple accumulation. Events emmitted by the queue can be
-- accumulated to produce a value or to be placed onto a new queue
accumulate :: (b -> a -> b) -> b -> PtQueue a -> b
accumulate f a (PtQueue s) = Foldable.foldl' (\a' e' -> f a' (message e')) a s

delay :: PtQueue a -> PtQueue (a,a)
delay (PtQueue s) = PtQueue (Seq.zipWith (_zipEventsWith (,)) s (Seq.drop 1 s))

delayWith :: (a -> a -> b) -> PtQueue a -> PtQueue b
delayWith f (PtQueue s) = PtQueue (Seq.zipWith (_zipEventsWith f) s (Seq.drop 1 s))

_zipEventsWith :: (a -> a -> b) -> PtEvent a -> PtEvent a -> PtEvent b
_zipEventsWith f (PtEvent _ x) (PtEvent t y) = mkEvent t (f x y)

_sortEvents :: (Seq (PtEvent a)) -> (Seq (PtEvent a))
_sortEvents = Seq.sortBy (compare `on` _timestamp)

_until :: PtQueue a -> POSIXTime -> PtQueue a
_until (PtQueue xs) t = PtQueue (Seq.takeWhileL ((t >=) . _timestamp) (_sortEvents xs))

