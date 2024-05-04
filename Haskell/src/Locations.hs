module Locations where 

import Items

data Direction = Forward | Back | Left | Right deriving (Eq)

data Path = Path{
    direction::Direction,
    to::Location
} deriving(Eq)

data Location = Location{
    name::String,
    description::String,
    items::[Item],
    pahts::[Path]
} deriving(Eq)