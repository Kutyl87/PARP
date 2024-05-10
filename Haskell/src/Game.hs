module Game where

import qualified Items
import qualified Locations
import Control.Exception

import qualified Data.Map (Map, lookup, fromList)
import Data.Maybe (isNothing, fromMaybe)
import Locations (strToDir)

data Event = RatKingDefeated deriving Eq

data GameState = GameState{
    inventory::[Items.Item],
    currentLocation::String,
    message::String,
    locations::Data.Map.Map String Locations.Location,
    events::[Event]
}

initGameState::GameState
initGameState = GameState
    []
    "Entrance"
    "You are at the entrance"
    (Data.Map.fromList [("Entrance", Locations.entrance), 
    ("Tunnel 1", Locations.tunnel_1)])
    []

describe::GameState->String->GameState
describe gs s = gs {message = fromMaybe "You are not holding this item!" (Items.getItemDescription (inventory gs) s)}

getCurLocation::GameState->Locations.Location
getCurLocation gs = fromMaybe Locations.entrance (Data.Map.lookup (currentLocation gs) (locations gs))

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

                
