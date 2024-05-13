module Types where

import Data.Map (Map)

data Direction = Forward | Back | Left | Right deriving (Eq, Ord)

data Location = Location{
    name::String,
    description::GameState->String,
    items::Data.Map.Map String Int,
    paths::Map Direction String
}

data Event = RatKingDefeated deriving Eq

data GameState = GameState{
    inventory::Data.Map.Map String Int,
    currentLocation::String,
    message::String,
    locations::Data.Map.Map String Location,
    events::[Event],
    energy::Int,
    maxEnergy::Int,
    dead::Bool
}