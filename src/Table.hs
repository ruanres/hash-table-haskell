module Table 
(
  get,
  insert,
  contain,
  search,
  removeAll,
  remove
) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)


{- 
  Get the value associated with a key, 
  if there is no value associated with the key
  then it return an empty list 
-}
get key table = fromMaybe [] value
    where value = search key table

{-
	Inserts a new value to the table,
	if the key already exists then its associated value is updated
-}
insert key value table = M.insertWith (++) key [value] table

{-
  Verifies if the pair key value exists
-}
contain key value table = elem value (get key table) 

{-
  If the key is present it return Just the value,
  otherwise Nothing is returned
-}
search key table = M.lookup key table

{-
  Removes all values associated with the given key
-}
removeAll key table = M.delete key table

{-
  Remove the given value associate with the given key
-}
remove key value table = M.adjust (filter (\v -> v /= value)) key table

