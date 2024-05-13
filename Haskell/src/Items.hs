module Items where

import Data.Map (Map)
import qualified Data.Map as Map

note :: String
note = "note"

stone_tablet_half :: String
stone_tablet_half = "Stone tablet half"

stone_tablet :: String
stone_tablet = "Stone tablet"

recpies :: Map String [String]
recpies = Map.fromList [
    (stone_tablet, [stone_tablet_half, stone_tablet_half])]

descriptions :: Map String String
descriptions = Map.fromList [
    (note, "A note on the ground")]

printItemList::[(String, Int)]->String
printItemList [] = ""
printItemList (x:xs) = fst x ++ " (count: " ++ show (snd x) ++ ")\n" ++ printItemList xs
