{- |
Module      : Number.Aggregate.Get
Description : Obtain the current aggregated number.
Copyright   : (c) 2019 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Number.Aggregate.Start provides a set of functions to initialize the process of
aggregation of a stream of numbers.
-}

module Number.Aggregate.Get
( get
) where

import Data.Word

import qualified Number.Aggregate.Type as A


-- | 
getVariance :: Floating a
  => Word64   -- ^ count
  -> a       -- ^ second moment
  -> Maybe a -- ^ variance
getVariance cnt mom
  | cnt == 0  = Nothing
  | cnt == 1  = Just 0.0
  | otherwise = Just $ mom / fromIntegral cnt

-- | Obtain the current aggregated value. Due to the nature of some of the
-- aggregate functions, it is possible that a value has not been computed due
-- to insufficient input size, e.g. only a single value was provided, which
-- does not have a variance.
get :: Floating a
  => A.Aggregate a -- ^ aggregate
  -> Maybe a       -- ^ value
get (A.Average _ cur)        = cur
get (A.Variance cnt _ mom) = getVariance cnt mom
get (A.Minimum val)          = val
get (A.Maximum val)          = val
get (A.Count cnt)            = Just $ fromIntegral cnt 
get (A.Sum cur)              = Just cur 
get (A.First cur)            = cur 
get (A.Last cur)             = cur 
