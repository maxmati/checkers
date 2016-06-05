module Utils where

import Data.Maybe

removeEmpty :: [(t, Maybe a)] -> [(t, a)]
removeEmpty list = let removeJust (a,b) = (a, fromJust b)
                       isNotEmpty (_, b) = isJust b
                   in  map removeJust $ filter isNotEmpty list