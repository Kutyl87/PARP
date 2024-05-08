module Items where

import Data.Map (Map)
-- import Data.Typeable (typeOf)
import Data.List (isInfixOf)
import qualified Data.Map as Map


data Item = Item{
    name::String,
    description::String
} deriving(Eq)

recpies :: Map String [String]
recpies = Map.fromList [("Stone tablet", ["Stone tablet half", "Stone tablet half"])]


note :: Item
note = Item
    "Note"
    "A note"

placeholderItem::Item
placeholderItem = Item
    "Placeholder item"
    "Placeholder item"

-- TODO: Test functions
countItems::[Item]->String->Int
countItems [] _ = 0
countItems (x:xs) s =
    if s `isInfixOf` name x
        then 1+countItems xs s
        else countItems xs s

-- TODO: Function that returns an item with given name
getItem::[Item]->String->Maybe Item
getItem [] _ = Nothing
getItem xs _ = Just (head xs) 
-- getItem arr s = head 

getItemDescription::[Item]->String->Maybe String
getItemDescription [] _ = Nothing
getItemDescription (x:xs) s =
    if s `isInfixOf` name x
        then Just (description x)
        else getItemDescription xs s