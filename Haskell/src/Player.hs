module Player where

import qualified Items
import qualified Locations

import Data.Maybe (isNothing, fromMaybe, fromJust)

data Player = Player{
    inventory::[Items.Item],
    currentLocation::Locations.Location
}

describe::Player->String->String
describe p s = fromMaybe "You are not holding this item!" (Items.getItemDescription (inventory p) s) 

-- TODO: Function to pick up items
pickUp::Player->String->String
pickUp p s = do
    let h = Items.getItem (Locations.items (currentLocation p)) s in
        if isNothing h then return "This is item is not here!"
        else inventory p ++ fromMaybe h 
        return "Added"
    -- let h = Items.getItem (Locations.items (currentLocation p)) s in
    --     if isNothing h then "This is item is not here!"
    --     