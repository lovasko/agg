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
data Aggregate a
 = Average  Word64 (Maybe a) -- count, average
 | Minimum  (Maybe a)        -- minimum
 | Maximum  (Maybe a)        -- maximum
 | Sum      a                -- sum
 | Count    Word64                -- count
 | Variance Word64 (Maybe a) a -- count, average, second moment
 | First    (Maybe a)        -- first
 | Last     (Maybe a)        -- last
