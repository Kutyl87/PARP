module Game where

import qualified Items
import qualified Locations

import Data.Maybe (fromMaybe)

data Event = RatKingDefeated deriving Eq

data GameState = GameState{
    inventory::[Items.Item],
    currentLocation::Locations.Location,
    message::String,
    locations::[Locations.Location],
    events::[Event]
}

initGameState::GameState
initGameState = GameState
    []
    Locations.entrance
    "You are at the entrance"
    []
    []



describe::GameState->String->GameState
describe p s = p {message = fromMaybe "You are not holding this item!" (Items.getItemDescription (inventory p) s)}
-- describe p s = 

-- -- TODO: Function to pick up items
-- pickUp::GameState->String->GameState
-- pickUp p s = 
--     -- let h = Items.getItem (Locations.items (currentLocation p)) s in
--     --     if isNothing h then "This is item is not here!"
--     --     