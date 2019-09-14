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
( first
, last
, minimum
, maximum
, count
, sum
, average
, variance 
) where

import Prelude hiding (first, last, minimum, maximum, sum)
import qualified Number.Aggregate.Type as A


-- | Start aggregating the first number in a stream.
first
  :: A.Aggregate -- ^ aggregate
first = A.First Nothing

-- | Start aggregating the last number in a stream.
last
  :: A.Aggregate -- ^ aggregate
last = A.Last Nothing

-- | Start aggregating the minimal number of the stream.
minimum
  :: A.Aggregate -- ^ aggregate
minimum = A.Minimum Nothing

-- | Start aggregating the maximal number of the stream.
maximum
  :: A.Aggregate -- ^ aggregate
maximum = A.Maximum Nothing

-- | Start counting the numbers in the stream.
count
  :: A.Aggregate -- ^ aggregate
count = A.Count 0

-- | Start aggregating the sum of numbers in the stream.
sum
  :: A.Aggregate -- ^ aggregate
sum = A.Sum 0

-- | Start aggregating the average of a stream of numbers.
average
  :: A.Aggregate -- ^ aggregate
average = A.Average 0 Nothing

-- | Start aggregating the variance of a stream of numbers.
variance
  :: A.Aggregate -- ^ aggregate
variance = A.Variance 0 Nothing 0
