{-
	Hash table closed address
-}

module HashTableClosedAddress
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
  Constant used to indicate 
  an invalid position on the table
-}
notFound = -1


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
contain value table = (search value table) /= notFound


{- 
  Get the value associated with a key, 
  if it exists then Just it is returned,
  otherwise Nothing is returned 
-}
search value table
  | elem value (getValues key table) = key
  | otherwise = notFound
  where key = hash value


{-
	Get the values associated with the given key
-}
getValues key table = fromMaybe [] (M.lookup key table)
