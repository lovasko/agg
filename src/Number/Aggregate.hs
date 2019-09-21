{- |
Module      : Number.Aggregate
Description : Aggregate a stream of values into a single one.
Copyright   : (c) 2019 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Number.Aggregate exports a single type that represents an aggregation of a
stream of values of the given parametric type. Along with it, a number of
constructor-like functions that start the aggregation, accompanied by two other
functions to update the aggregate and to retrieve it.

The proposed convention on importing this module:
  > import qualified Number.Aggregate as A
-}

module Number.Aggregate (
-- Container.
  Aggregate   -- *           -> *

-- Core functionality.
, get       -- Aggregate a -> Maybe a
, update    -- a           -> Aggregate a -> Aggregate a

-- Constructors.
, minimum   -- Aggregate a
, maximum   -- Aggregate a
, first     -- Aggregate a
, last      -- Aggregate a
, count     -- Aggregate a
, sum       -- Aggregate a
, average   -- Aggregate a
, variance  -- Aggregate a
) where

import Prelude()

import Number.Aggregate.Get
import Number.Aggregate.Start
import Number.Aggregate.Type
import Number.Aggregate.Update
