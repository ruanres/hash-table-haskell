{-
	Hash table open address linear probing
-}

module HashTableOpenAddressLinearProbing
(
  size,
  hash,
  new,
  insert,
  remove,
  contain,
  search,
  currentSize,
  isFull
) where

import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust)
-- import Data.IORef (newIORef, writeIORef, readIORef)

{-
  Constant used to indicate 
  an invalid position on the table
-}
notFound = -1

{-
  Initial prob value user to
  calculate the hash of a value
-}
initialProb = 0

{-
  Constant used to indicate 
  a deleted position on the table
-}
deleted = "DELETED_SLOT"

{-
  Max table size
-}
size = 3


{-
	Calculate the key of a value using the given prob
-}
hash value prob = ((length value) + prob) `mod` (size)


{-
	Create a new table
-}
new _ = M.empty


{-
  Inserts a new value in the table, if the key already exists
  then try to put the value in the next free position.
  Otherwise, if the table is full an error thrown
-}
insert value table = insertProbing value initialProb table

insertProbing value prob table
  | isFull table = error "Table full"
  | isDeleted val = M.insert key value table
  | otherwise = insertProbing value (prob + 1) table
  where 
    key = hash value prob
    val = get key table


{-
  Remove the given value if present in the table
-}
remove value table = removeProbing value initialProb table

removeProbing value prob table
  | (isDeleted val) || (prob == size) = table
  | (fromJust val) == value = M.adjust setDeleted key table
  | otherwise = removeProbing value (prob + 1) table
  where
    setDeleted = \_ -> deleted
    key = hash value initialProb
    val = get key table


{-
  Verifies if the value exists
-}    
contain value table = (search value table) /= notFound


{- 
	Returns the value key if it
	is present in the table. Otherwise,
	an invalid position is returned
-}
search value table = searchProbing value initialProb table

searchProbing value prob table
  | (isDeleted val) || (prob == size) = notFound
  | value == (fromJust val) = key
  | otherwise = searchProbing value (prob + 1) table
  where 
    key = hash value prob
    val = get key table


{-
	Get the value associated with the given key
-}
get key table = M.lookup key table


{-
  Checks whether the given value is empty or deleted
-}
isDeleted val = isNothing val || (fromJust val) == deleted 


{-
  Check if the table has reached its max size
-}
isFull table = size == (currentSize table)


{-
  Returns the current table size
-}
currentSize table = M.foldl (+) 0 . M.map toOne . M.filter notDeleted $ table
  where
    toOne = \_ -> 1
    notDeleted = \v -> v /= deleted
