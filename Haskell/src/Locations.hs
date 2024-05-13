module Locations where 

import Data.Map (Map, fromList, toList, lookup, empty)
import Items

data Direction = Forward | Back | Left | Right deriving (Eq, Ord)

strToDir::String->Maybe Direction
strToDir s | s == "forward" = Just Locations.Forward
        | s == "back" = Just Locations.Back
        | s == "left" = Just Locations.Left
        | s == "right" = Just Locations.Right
        | otherwise = Nothing

getLocationStringAtDir::Location->Direction->Maybe String
getLocationStringAtDir l d = Data.Map.lookup d (paths l)
    
data Location = Location{
    name::String,
    description::String,
    items::Data.Map.Map String Int,
    paths::Map Direction String
}

listItems::Location->String
listItems l = Items.printItemList (Data.Map.toList (Locations.items l))


entrance :: Location
entrance = Location
    "Entrance"
    "The entrance to the tunnel"
    (Data.Map.fromList [(Items.note, 1)])
    (Data.Map.fromList [(Locations.Forward, "Tunnel 1")])

tunnel_1 :: Location
tunnel_1 = Location
    "Tunnel 1"
    "Tunnel 1"
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Entrance")])
