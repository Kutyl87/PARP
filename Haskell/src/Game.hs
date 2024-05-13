module Game where

import qualified Items
import qualified Locations
import qualified Data.Map (Map, lookup, fromList, toList, empty, insert, delete, member)
import Data.Maybe (isNothing, fromMaybe, fromJust, maybe)
import Locations (strToDir)
import System.Random (randomRIO)
import Control.Monad.State
import Types
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
data Event = RatKingDefeated deriving Eq


dummyGameState::Types.GameState
dummyGameState = Types.GameState
    Data.Map.empty
    "Entrance"
    ""
    (Data.Map.fromList [("Entrance", Locations.entrance)])
    []
    100
    100
    False
    10

initGameState::Types.GameState
initGameState = Types.GameState
    Data.Map.empty
    "Entrance"
    (Types.description Locations.entrance dummyGameState)
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
    100
    False
    10

describe::Types.GameState->String->Types.GameState
describe gs s = gs {message = maybe "You are not holding this item!" (const (fromMaybe "" (Data.Map.lookup s Items.descriptions))) (Data.Map.lookup s (inventory gs))}

take::Types.GameState->String->Types.GameState
take gs s = do
    let inum = Data.Map.lookup s (Types.items (getCurLocation gs))
    if fromMaybe 0 inum == 0 then gs {message = "This item is not here!"}
    else do
        let newLocationItems = do {
            if fromJust inum > 1 then Data.Map.insert s (fromJust inum - 1)  (Types.items (getCurLocation gs))
            else Data.Map.delete s (Types.items (getCurLocation gs))
        }
        let newLocation = (getCurLocation gs) {Types.items=newLocationItems}
        let ninum = Data.Map.lookup s (inventory gs)
        if isNothing ninum then gs {message = "Picked up "++s, locations=Data.Map.insert (currentLocation gs) newLocation  (locations gs), inventory=Data.Map.insert s 1 (inventory gs)}
        else gs {message = "Picked up "++s, locations=Data.Map.insert  (currentLocation gs) newLocation (locations gs), inventory=Data.Map.insert s (fromJust ninum + 1) (inventory gs)}

fight :: GameState -> String -> GameState
fight gs s = 
    if s /= Items.aligator then gs { message = "You can't fight with anything other than an aligator!" }
    else do
        let curLocation = currentLocation gs
        if not (Locations.isAligatorInLocation curLocation (locations gs)) then
            gs { message = "There is no aligator in this location!" }
        else do
            let e = energy gs
            let eLoss = unsafePerformIO $ randomRIO (0, 50)
            let newE = e - eLoss
            let newLocations = Locations.removeAligatorFromLocation curLocation (locations gs)
            if newE <= 0 then gs { energy = 0, message = "You are out of energy. You have died." }
            else do
                let newGs = improveResting gs 
                newGs { energy = newE, locations = newLocations, message = (message newGs) ++ "\nYou have fought with an aligator. You have " ++ show newE ++ " energy left." }

buy :: GameState -> String -> GameState
buy gs s = 
    let locationName = case Data.Map.lookup (currentLocation gs) (locations gs) of
            Just loc -> Types.name loc
            Nothing -> ""
        hasFlute = Data.Map.member Items.flute (inventory gs)
    in if locationName /= (Types.name Locations.dealer_room)
        then gs { message = "You are not in the dealer's room." }
        else if s /= Items.flute 
            then gs { message = "You can't buy anything other than a flute!" }
            else if energy gs < 50 
                then gs { message = "You don't have enough energy." }
            else if hasFlute
                then gs { message = "You already have a flute." }
                else let newE = energy gs - 50
                         newInventory = Data.Map.insert Items.flute 1 (inventory gs)
                     in gs { energy = newE, inventory = newInventory, message = "You have bought a magic flute. You have " ++ show newE ++ " energy left." }

improveResting :: Types.GameState->Types.GameState
improveResting gs = do
    let newRp = (Types.restingPace gs) + 10
    gs {message = "You have improved your resting pace. It is now " ++ show newRp ++ "", restingPace = newRp}

printInventory::Types.GameState->Types.GameState
printInventory gs = let newInventory = Items.cleanInventory (inventory gs) in gs {message = "Inventory:\n" ++ Items.printItemList (Data.Map.toList newInventory), inventory=newInventory}

getCurLocation::Types.GameState->Types.Location
getCurLocation gs = fromJust (Data.Map.lookup (currentLocation gs) (locations gs))

go::Types.GameState->String->Types.GameState
go gs ds = do
            let d = Locations.strToDir ds
            let l = getCurLocation gs
            if isNothing d then gs {message = "Incorrect direction"}
            else do
                let nls = Locations.getLocationStringAtDir l (fromMaybe Types.Forward d)
                if isNothing nls then gs {message = "You cannot go there!"}
                else do
                    if (energy gs) <= 10 then gs {message = "You are out of energy.\nYou died. Game over.", dead = True}
                    else
                        let nl = fromMaybe Locations.entrance (Data.Map.lookup (fromMaybe "" nls) (locations gs)) in
                        gs {message = Types.description nl gs ++ "\n Energy: " ++ show (energy gs - 10), currentLocation = Types.name nl, energy = energy gs - 10}

rest::Types.GameState->Types.GameState
rest gs =  do
    let newEnergy = if (energy gs + (Types.restingPace gs)) < maxEnergy gs then energy gs + (Types.restingPace gs) else maxEnergy gs
    gs {energy = newEnergy, message="You take a rest and feel better. Your energy is now " ++ show newEnergy}
look::Types.GameState->Types.GameState
look gs = gs {message = Types.description (getCurLocation gs) gs ++ "\nItems in current location:\n" ++ Locations.listItems (getCurLocation gs)}

craft::GameState->String->GameState
craft gs s = let recipe = Data.Map.lookup s Items.recpies in
    if isNothing recipe then gs {message="You cannot craft this!"}
    else if Items.checkRecipeItems (inventory gs) (fromJust recipe) then
        gs {inventory=Data.Map.insert s 1 (Items.subtractRecipeItems (inventory gs) (fromJust recipe)), message="Crafted "++s}
        else gs {message="You don't have the required items!"}