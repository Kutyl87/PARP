module Game where

import qualified Items
import qualified Locations
import qualified Data.Map (Map, lookup, fromList, toList, empty, insert, delete)
import Data.Maybe (isNothing, fromMaybe, fromJust, maybe)
import Locations (strToDir)

data Event = RatKingDefeated deriving Eq

data GameState = GameState{
    inventory::Data.Map.Map String Int,
    currentLocation::String,
    message::String,
    locations::Data.Map.Map String Locations.Location,
    events::[Event],
    energy::Int,
    dead::Bool
}

initGameState::GameState
initGameState = GameState
    Data.Map.empty
    "Entrance"
    (Locations.description Locations.entrance)
    (Data.Map.fromList [("Entrance", Locations.entrance),
                        ("Hall", Locations.hall),
                        ("In front of first tunnel", Locations.in_front_of_first_tunnel),
                        ("In front of second tunnel", Locations.in_front_of_second_tunnel),
                        ("In front of third tunnel", Locations.in_front_of_third_tunnel),
                        ("First tunnel", Locations.first_tunnel),
                        ("Dealer room", Locations.dealer_room),
                        ("Aligator room", Locations.aligator_room),
                        ("End of first tunnel", Locations.end_of_first_tunnel),
                        ("Waterfall", Locations.waterfall),
                        ("Second tunnel", Locations.second_tunnel),
                        ("Tunnel diggers", Locations.tunnel_diggers),
                        ("Side tunnel", Locations.side_tunnel),
                        ("Second tunnel 1", Locations.second_tunnel_1),
                        ("Third tunnel", Locations.third_tunnel),
                        ("Synagogue", Locations.synagogue)])
    []
    100
    False

describe::GameState->String->GameState
describe gs s = gs {message = maybe "You are not holding this item!" (const (fromMaybe "" (Data.Map.lookup s Items.descriptions))) (Data.Map.lookup s (inventory gs))}

take::GameState->String->GameState
take gs s = do
    let inum = Data.Map.lookup s (Locations.items (getCurLocation gs))
    if fromMaybe 0 inum == 0 then gs {message = "This item is not here!"}
    else do
        let newLocationItems = do {
            if fromJust inum > 1 then Data.Map.insert s (fromJust inum - 1)  (Locations.items (getCurLocation gs))
            else Data.Map.delete s (Locations.items (getCurLocation gs))
        }
        let newLocation = (getCurLocation gs) {Locations.items=newLocationItems}
        let ninum = Data.Map.lookup s (inventory gs)
        if isNothing ninum then gs {message = "Picked up "++s, locations=Data.Map.insert (currentLocation gs) newLocation  (locations gs), inventory=Data.Map.insert s 1 (inventory gs)}
        else gs {message = "Picked up "++s, locations=Data.Map.insert  (currentLocation gs) newLocation (locations gs), inventory=Data.Map.insert s (fromJust ninum + 1) (inventory gs)}

printInventory::GameState->GameState
printInventory gs = gs {message = "Inventory:\n" ++ Items.printItemList (Data.Map.toList (inventory gs))}

getCurLocation::GameState->Locations.Location
getCurLocation gs = fromJust (Data.Map.lookup (currentLocation gs) (locations gs))

go::GameState->String->GameState
go gs ds = do
            let d = Locations.strToDir ds
            let l = getCurLocation gs
            if isNothing d then gs {message = "Incorrect direction"}
            else do
                let nls = Locations.getLocationStringAtDir l (fromMaybe Locations.Forward d)
                if isNothing nls then gs {message = "You cannot go there!"}
                else do
                    if (energy gs) <= 10 then gs {message = "You are out of energy.\nYou died. Game over.", dead = True}
                    else
                        let nl = fromMaybe Locations.entrance (Data.Map.lookup (fromMaybe "" nls) (locations gs)) in
                        gs {message = Locations.description nl ++ "\n Energy: " ++ show (energy gs - 10), currentLocation = Locations.name nl, energy = energy gs - 10}

look::GameState->GameState
look gs = gs {message = Locations.description (getCurLocation gs) ++ "\nItems in current location:\n" ++ Locations.listItems (getCurLocation gs)}

craft::GameState->String->GameState
craft gs s = let recipe = Data.Map.lookup s Items.recpies in
    if isNothing recipe then gs {message="You cannot craft this!"}
    else if Items.checkRecipeItems (inventory gs) (fromJust recipe) then
        gs {inventory=Data.Map.insert s 1 (Items.subtractRecipeItems (inventory gs) (fromJust recipe))}
        else gs {message="You don't have the required items!"}