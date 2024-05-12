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
    itemList::Data.Map.Map String Items.Item,
    events::[Event]
}

initGameState::GameState
initGameState = GameState
    Data.Map.empty
    "entrance"
    "You are at the entrance"
    (Data.Map.fromList [("entrance", Locations.entrance),
    ("Tunnel 1", Locations.tunnel_1)])
    (Data.Map.fromList [("note", Items.note)])
    []

describe::GameState->String->GameState
describe gs s = gs {message = maybe "You are not holding this item!" Items.description (Data.Map.lookup s (itemList gs))}

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
look gs = gs {message = Locations.description (getCurLocation gs)}

