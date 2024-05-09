module Locations where 

import Items ( note, Item )
import Data.Map (Map, fromList, lookup)
import Data.List (isInfixOf)
import Data.Text (toLower)

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
    items::[Item],
    paths::Map Direction String
}



entrance :: Location
entrance = Location
    "Entrance"
    "The entrance to the tunnel"
    [Items.note]
    (Data.Map.fromList [(Locations.Forward, "Tunnel 1")])

tunnel_1 :: Location
tunnel_1 = Location
    "Tunnel 1"
    "Tunnel 1"
    []
    (Data.Map.fromList [(Locations.Back, "Entrance")])