{- |
Module      : Number.Aggregate.Start
Description : Start point for aggregation.
Copyright   : (c) 2019 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Number.Aggregate.Start provides a set of functions to initialize the process of
aggregation of a stream of numbers.
-}

module Number.Aggregate.Start
( first    -- Aggregate a
, last     -- Aggregate a
, count    -- Aggregate a
, sum      -- Aggregate a
, minimum  -- Aggregate a
, maximum  -- Aggregate a
, average  -- Aggregate a
, variance -- Aggregate a
) where

import Prelude hiding (first, last, minimum, maximum, sum)
import qualified Number.Aggregate.Type as A


-- | Start aggregating the first number in a stream.
first :: Floating a
  => A.Aggregate a -- ^ aggregate
first = A.First Nothing

-- | Start aggregating the last number in a stream.
last :: Floating a
  => A.Aggregate a -- ^ aggregate
last = A.Last Nothing

-- | Start counting the numbers in the stream.
count :: Floating a
  => A.Aggregate a -- ^ aggregate
count = A.Count 0

-- | Start aggregating the sum of numbers in the stream.
sum :: Floating a
  => A.Aggregate a -- ^ aggregate
sum = A.Sum 0

-- | Start aggregating the minimal number of the stream.
minimum :: Floating a
  => A.Aggregate a -- ^ aggregate
minimum = A.Minimum Nothing

-- | Start aggregating the maximal number of the stream.
maximum :: Floating a
  => A.Aggregate a -- ^ aggregate
maximum = A.Maximum Nothing

-- | Start aggregating the average of a stream of numbers.
average :: Floating a
  => A.Aggregate a -- ^ aggregate
average = A.Average 0 Nothing

-- | Start aggregating the variance of a stream of numbers.
variance :: Floating a
  => A.Aggregate a -- ^ aggregate
variance = A.Variance 0 Nothing 0
