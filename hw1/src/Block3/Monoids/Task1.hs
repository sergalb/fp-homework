module Block3.Monoids.Task1 where

import Data.Maybe (fromMaybe)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldMap (fromMaybe [])