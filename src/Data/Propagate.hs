-- {-# LANGUAGE FlexibleInstances #-}

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
 ,timestamp
  -- * Working with the Queue
 ,filter
 ,union
 ,fold
 ,delay
 ) where

import           Data.Function
import qualified Data.List             as List
import           Data.Time.Clock.POSIX
import           Prelude               hiding (filter, map)

-- | The Queue holding the events
newtype PtQueue a = PtQueue [PtEvent a] deriving (Eq,Show)

-- | The Event to be propagated
data PtEvent a = PtEvent POSIXTime a deriving (Eq,Show)


instance Functor PtQueue where
  -- ^ Useful for changing of messages in the queue. Provides fmap
  fmap f (PtQueue s) = PtQueue (List.map (fmap f) s)

instance Functor PtEvent where
  -- ^ Useful for changing of messages in the queue. Provides fmap
  fmap f (PtEvent t a) = PtEvent t (f a)

-- | Empty queue.
--
-- > propagate (mkEvent now "New Sample") empty
--
empty :: PtQueue a
empty = PtQueue []

-- | Place an event onto a queue.
--
-- > propagate (mkEvent now "Another Sample") q
--
propagate :: () => PtEvent a -> PtQueue a -> PtQueue a
propagate e (PtQueue s) = PtQueue (_sortEvents (e : s))

fromList :: [PtEvent a] -> PtQueue a
fromList = PtQueue . _sortEvents

toList :: PtQueue a -> [PtEvent a]
toList (PtQueue s) = s

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
union (PtQueue s1) (PtQueue s2) = PtQueue (_sortEvents (s1 ++ s2))

leftMerge :: PtQueue a -> PtQueue b -> PtQueue (a,PtQueue b)
leftMerge (PtQueue xs) ys = PtQueue (List.map (adjustedTo ys) xs)
  where
  adjustedTo :: PtQueue z -> PtEvent x -> PtEvent (x,PtQueue z)
  adjustedTo zs (PtEvent t x) = PtEvent t (x, zs `_until` t)

filter :: (a -> Bool) -> PtQueue a -> PtQueue a
filter = undefined

-- | A simple accumulation. Events emmitted by the queue can be
-- accumulated to produce a value or to be placed onto a new queue
fold :: (b -> a -> b) -> b -> PtQueue a -> b
fold = undefined

-- | Delay is a special case of fold which produces a queue with
-- several messages brought together into a list
delay :: Int -> PtQueue a -> PtQueue [a]
delay = undefined

timestamp :: PtEvent a -> POSIXTime
timestamp (PtEvent t _) = t

message :: PtEvent a -> a
message (PtEvent _ m) = m

mkEvent :: POSIXTime -> a -> PtEvent a
mkEvent t a = PtEvent t a

_sortEvents :: [PtEvent a] -> [PtEvent a]
_sortEvents = List.sortBy (compare `on` timestamp)

_until :: PtQueue a -> POSIXTime -> PtQueue a
_until (PtQueue xs) t = PtQueue (List.filter ((t >=) . timestamp) xs)

