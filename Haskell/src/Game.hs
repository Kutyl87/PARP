module Game where

import qualified Items
import qualified Locations
import qualified Data.Map (Map, lookup, fromList, empty, insert)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Locations (strToDir)

data Event = RatKingDefeated deriving Eq

data GameState = GameState{
    inventory::Data.Map.Map String Int,
    currentLocation::String,
    message::String,
    locations::Data.Map.Map String Locations.Location,
    itemList::Data.Map.Map String Int,
    events::[Event]
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

    (Data.Map.fromList [(Items.note, 1)])
    []

describe::GameState->String->GameState
describe gs s = gs {message = maybe "You are not holding this item!" (const (fromMaybe "" (Data.Map.lookup s Items.descriptions))) (Data.Map.lookup s (itemList gs))}

take::GameState->String->GameState
take gs s = do
    let inum = Data.Map.lookup s (Locations.items (getCurLocation gs))
    if fromMaybe 0 inum == 0 then gs {message = "This item is not here!"}
    else do
        let newLocationItems = Data.Map.insert s (fromJust inum - 1)  (Locations.items (getCurLocation gs))
        let newLocation = (getCurLocation gs) {Locations.items=newLocationItems}
        let ninum = Data.Map.lookup s (inventory gs)
        if isNothing ninum then gs {message = "Picked up "++s, locations=Data.Map.insert (currentLocation gs) newLocation  (locations gs), inventory=Data.Map.insert s 0 (inventory gs)}
        else gs {message = "Picked up "++s, locations=Data.Map.insert  (currentLocation gs) newLocation (locations gs), inventory=Data.Map.insert s (fromJust ninum) (inventory gs)}

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
                else let nl = fromMaybe Locations.entrance (Data.Map.lookup (fromMaybe "" nls) (locations gs)) in
                    gs {message = Locations.description nl, currentLocation = Locations.name nl}

look::GameState->GameState
look gs = gs {message = Locations.description (getCurLocation gs) ++ "\nItems in current location:\n" ++ Locations.listItems (getCurLocation gs)}

