{- |
Module      : Number.Aggregate.Update
Description : Update existing aggregate based on a new value.
Copyright   : (c) 2019 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Number.Aggregate.Update implements a function that recomputes the aggregate of
the stream by incorporating the new value.
-}

module Number.Aggregate.Update
( update -- a -> Aggregate a -> Aggregate a
) where

import Data.Word

import qualified Number.Aggregate.Type as A


-- | Update the average of all values.
updateAverage :: Floating a
  => a             -- ^ new value
  -> Maybe a       -- ^ old average
  -> Word64        -- ^ population count
  -> A.Aggregate a -- ^ new average
updateAverage new Nothing    cnt = A.Average (cnt + 1) (Just new)
updateAverage new (Just cur) cnt = A.Average (cnt + 1) (Just avg')
  where
    cnt' = fromIntegral cnt
    avg' = (cnt' * cur + new) / (cnt' + 1.0)

-- | Update the minimum of all values.
updateMinimum :: (Floating a, Ord a)
  => a             -- ^ new value
  -> Maybe a       -- ^ current minimum
  -> A.Aggregate a -- ^ new aggregate
updateMinimum new Nothing      = A.Minimum (Just new)
updateMinimum new cur@(Just val)
  | new < val                  = A.Minimum (Just new)
  | otherwise                  = A.Minimum cur

-- | Utility function to update the aggregator of the maximal value.
updateMaximum :: (Floating a, Ord a)
  => a             -- ^ new value
  -> Maybe a       -- ^ current value
  -> A.Aggregate a -- ^ new value
updateMaximum new Nothing    = A.Maximum (Just new)
updateMaximum new cur@(Just val)
  | new > val                = A.Maximum (Just new)
  | otherwise                = A.Maximum cur

-- | Update the variance of all values. Due to the design goal of the module -
-- to allow for computation of a select subset of aggregate functions, the
-- variance function needs to compute its own average. This is a known (and
-- accepted) downside of the module.
updateVariance :: Floating a
  => a             -- ^ new value
  -> Word64        -- ^ value count
  -> Maybe a       -- ^ current average
  -> a             -- ^ second moment
  -> A.Aggregate a -- ^ aggregate
updateVariance new cnt Nothing    mom = A.Variance (cnt + 1) (Just new)  0.0
updateVariance new cnt (Just avg) mom = A.Variance (cnt + 1) (Just avg') mom'
  where
    diff = new - avg
    mom' = mom + diff * diff 
    avg' = (cnt' * avg + new) / (cnt' + 1.0)
    cnt' = fromIntegral cnt

-- | Update the number of all seen values.
updateCount :: Floating a
  => Word64        -- ^ current count
  -> A.Aggregate a -- ^ new aggregate
updateCount cnt = A.Count (cnt + 1)

-- | Update the sum of all seen values.
updateSum :: Floating a
  => a             -- ^ new value
  -> a             -- ^ current value
  -> A.Aggregate a -- ^ aggregate
updateSum new cur = A.Sum (cur + new)

-- | Update the first value seen.
updateFirst :: Floating a
  => a             -- ^ new value
  -> Maybe a       -- ^ current value
  -> A.Aggregate a -- ^ new aggregate
updateFirst new Nothing = A.First (Just new)
updateFirst new cur     = A.First cur

-- | Update the last value seen. The current value is irrelevant as the
-- aggregate of the last value always only updates the value.
updateLast :: Floating a
  => a             -- ^ new value
  -> A.Aggregate a -- ^ new aggregate
updateLast new = A.Last (Just new)

-- | Update the current aggregated value. The function matches the aggregator
-- pattern, unpacks the internals and passes them onto a specialized function that
-- creates a new instance of the aggregator.
update :: (Floating a, Ord a)
  => a             -- ^ new value
  -> A.Aggregate a -- ^ old aggregate
  -> A.Aggregate a -- ^ new aggregate
update new (A.Average cnt cur)      = updateAverage  new cur cnt
update new (A.Minimum cur)          = updateMinimum  new cur 
update new (A.Maximum cur)          = updateMaximum  new cur
update _   (A.Count cnt)            = updateCount            cnt 
update new (A.Sum cur)              = updateSum      new cur
update new (A.Variance cnt avg mom) = updateVariance new cnt avg mom
update new (A.First cur)            = updateFirst    new cur 
update new (A.Last  _)              = updateLast     new
