import qualified Data.Map as M




-- insert k v M.empty = M.insert k v M.empty 
insert key value table = M.insert key [value] table


search key table = M.lookup key table


remove key table = M.delete key table


