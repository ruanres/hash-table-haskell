module HashTable 
(
  new,
  insert,
  remove,
  contain,
  search,
  size,
  hash
) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)


{-
  Max table size
-}
size = 30


{-
	Calculate the key of a value
-}
hash value = (length value) `mod` size 


{-
	Create a new table
-}
new _ = M.empty;


{-
	Inserts a new value to the table,
	if the key already exists then its associated value is updated
-}
insert value table = M.insertWith (++) (hash value) [value] table


{-
  Remove the given value associate with the given key
-}
remove value table = M.adjust (filter (\v -> v /= value)) (hash value) table


{-
  Verifies if the pair key value exists
-}
contain value table = elem value (fromMaybe [] item)
  where item = search value table


{- 
  Get the value associated with a key, 
  if it exists then Just it is returned,
  otherwise Nothing is returned 
-}
search value table = M.lookup (hash value) table
