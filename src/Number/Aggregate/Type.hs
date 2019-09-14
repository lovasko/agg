{- |
Module      : Number.Aggregate.Type
Description : Type representation of each aggregate function.
Copyright   : (c) 2019 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Number.Aggregate.Type defines the only type of the library - a representation
of the aggregate function with its internal state. Only the type itself should
be exported to the user, with all constructors to remain private.
-}

module Number.Aggregate.Type
( Aggregate(..)
) where

import Data.Word


-- | Aggregate function.
data Aggregate
 = Average  Word64 (Maybe Double) -- count, average
 | Minimum  (Maybe Double)        -- minimum
 | Maximum  (Maybe Double)        -- maximum
 | Sum      Double                -- sum
 | Count    Word64                -- count
 | Variance Word64 (Maybe Double) Double -- count, average, second moment
 | First    (Maybe Double)        -- first
 | Last     (Maybe Double)        -- last
