module Items where
import Data.Map (Map)
import qualified Data.Map as Map


data Item = Item{
    name::String,
    description::String
} deriving(Eq)

recpies = Map.fromList[("Stone tablet", ["Stone tablet half", "Stone tablet half"])]

note = Item
    "Note"
    "A note"