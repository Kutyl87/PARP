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

printItemList::[(String, Int)]->String
printItemList [] = ""
printItemList (x:xs) = fst x ++ " (count: " ++ show (snd x) ++ ")\n" ++ printItemList xs

listItems::Location->String
listItems l = printItemList (Data.Map.toList (Locations.items l))


entrance :: Location
entrance = Location
    "Entrance"
    "You are in the entrance. You notice doors behind you, and you feel pain inside your head. There is a small note at the floor. Maybe you should inspect it?"
    (Data.Map.fromList [(Items.note, 1)])
    (Data.Map.fromList [(Locations.Forward, "Hall")])

hall :: Location
hall = Location
    "Hall"
    "There is a hall with 3 tunnels. There must be a way to get out of there."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Entrance"), (Locations.Right, "In front of first tunnel"), (Locations.Forward, "In front of second tunnel"), (Locations.Left, "In front of third tunnel")])

in_front_of_first_tunnel :: Location
in_front_of_first_tunnel = Location
    "In front of first tunnel"
    "You are standing in front of the rightmost tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Hall"), (Locations.Forward, "First tunnel"), (Locations.Left, "In front of second tunnel")])

in_front_of_second_tunnel :: Location
in_front_of_second_tunnel = Location
    "In front of second tunnel"
    "You are standing in front of the middle tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Hall"), (Locations.Forward, "Second tunnel"), (Locations.Right, "In front of first tunnel"), (Locations.Left, "In front of third tunnel")])

in_front_of_third_tunnel :: Location
in_front_of_third_tunnel = Location
    "In front of third tunnel"
    "You are standing in front of the leftmost tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "Hall"), (Locations.Forward, "Third tunnel"), (Locations.Right, "In front of second tunnel")])

first_tunnel :: Location
first_tunnel = Location
    "First tunnel"
    "You are in the first tunnel."
    Data.Map.empty
    (Data.Map.fromList [(Locations.Back, "In front of first tunnel"), (Locations.Forward, "Dealer room")])

