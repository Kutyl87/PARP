module Items where

import Data.Map (Map)
import qualified Data.Map as Map


data Item = Item{
    name::String,
    description::String
} deriving(Eq)

recpies :: Map String [String]
recpies = Map.fromList [("Stone tablet", ["Stone tablet half", "Stone tablet half"])]

note :: Item
note = Item
    "note"
    "A note"

placeholderItem::Item
placeholderItem = Item
    "Placeholder item"
    "Placeholder item"