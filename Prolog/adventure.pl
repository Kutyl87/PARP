/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1, energy/1, max_energy/1, resting_pace/1, alive/1, travelling_cost/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(entrance).

path(entrance, explore_further, hall).
path(hall, visit_first_tunnel, first_tunnel).
path(hall, visit_second_tunnel, second_tunnel).
path(hall, visit_third_tunnel, third_tunnel).
path(first_tunnel, go_back_to_hall, hall).
path(first_tunnel, explore_further, dealer_room).
path(second_tunnel, go_back_to_hall, hall).
path(third_tunnel, go_back_to_hall, hall).
path(dealer_room, explore_further, aligator_room).
path(aligator_room, explore_further, waterfall).
at(note, entrance).
max_energy(100).
energy(100).
resting_pace(10).
travelling_cost(10).
change_max_energy(NewMax) :-
        retract(max_energy(_)),
        assert(max_energy(NewMax)).
/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules describe inspection of an object */
inspect(X) :-
        holding(X),
        describe(X),
        nl.

inspect(_) :-
        write('You aren''t holding it!'),
        nl.

/* These rules describe using an object - the key that is broken in half */
use(flute) :-
        holding(flute),
        i_am_at(aligator_room),
        write(' You have used a magic flute. Aligator is obey. You can use him as a form of transport.'), 
        nl.

use(flute) :- 
        holding(flute),
        write(' You have used a flute. Its sounds reverberate around you.'),
        nl.
use(X) :-
        holding(X),
        describe(X),
        nl.

use(_) :-
        write('You aren''t holding it!'),
        nl.
/* These rules define the direction letters as calls to go/1. */

explore_further :- go(explore_further).

go_back_to_hall :- go(go_back_to_hall).

visit_first_tunnel :- go(visit_first_tunnel).

visit_second_tunnel :- go(visit_second_tunnel).

visit_third_tunnel :- go(visit_third_tunnel).
/* This rule tells how to move in a given direction. */


go(Direction) :-
        i_am_at(Here),
        travelling_cost(C),
        energy(E),
        E > 0, 
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        NewE is E - C ,
        retract(energy(E)),
        assert(energy(NewE)),
        write('You have '), write(NewE), write(' energy left.'), nl,
        (NewE =:= 0 -> write('You are out of energy'), die ; true),
        !, look.

go(_) :-
    write('You can''t go that way or you are out of energy.'),
    nl.


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

improve_resting:-
        resting_pace(RP),
        NewRP is RP + 10,
        retract(resting_pace(RP)),
        assert(resting_pace(NewRP)),
        write('You have improved your resting pace. It is now '), write(NewRP), write('.'), nl.


drive_aligator:-
        write('You drive aligator. You do not spend energy. You can go to the next room.'), nl,
        retract(travelling_cost(_)),
        assert(travelling_cost(0)).
/* This rule tells how to die. */

die :-
        finish.

die :-
        write('You have died.  Game over.'),
        nl,
        retractall(i_am_at(_)),
        retractall(at(_, _)),
        retractall(holding(_)),
        retractall(energy(_)),
        retractall(max_energy(_)),
        retractall(alive(_)),
        assert(alive(no)),
        finish.
/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        halt.

/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('inspect(Object).   -- to inspect an object.'), nl,
        write('use(Object).       -- to use an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('finish.            -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(entrance) :- write('You are in the entrance. You notice doors behind you, and you feel pain inside your head. There is a small note at the floor. Maybe you should inspect it?'), nl.
describe(hall) :- write('There is a hall with 3 tunnels. There must be a way to get out of there.'), nl.
describe(first_tunnel) :- write('You are in the first tunnel. You can smell a blood inside. It seems to be a bad way to escape.'), nl.
describe(second_tunnel) :- write('You are in the second tunnel. You can see a light at the end of a tunnel. Maybe this is a way to escape?'), nl.
describe(third_tunnel) :- write('You cannot enter the doors. There is not a place to enter a key. Maybe a magic item can open them?'), nl.
describe(dealer_room) :- write('You have entered a Jew dealer space. He wants to sell you a magic flute, but he do not specified its aim. Maybe it can be useful? He wants to help him, it will cost you 60 energy.'), nl.
describe(aligator_room) :- write('You have entered an Aligator space. There is a huge reptile at the back. Fight could be difficult and demanding. Would you try?'), nl.
describe(waterfall) :- write('You have entered a waterfall. You can see a light at the end of the tunnel. You are carried away by the current of water.'), die, nl.
describe(note) :- write('You read the note from a lost wanderer. It says: "You are in a maze. You need to find a way out. There are 3 tunnels. The first one is very dangerous. The second one has a light at the end - that\'s the path you should take. The third one may hold a secret. Choose wisely."'), nl.
take_rest :-
        energy(E),
        max_energy(MaxE),
        resting_pace(RP),
        NewE is E + RP,
        NewE =< MaxE, 
        retract(energy(E)),
        assert(energy(NewE)),
        write('You take a rest and feel better. Your energy is now '), write(NewE), write('.'), nl.
energy_level :-
        energy(E),
        write('Your energy level is '), write(E), write('.'), nl.
show_max_energy_level :-
        max_energy(MaxE),
        write('Your maximum energy level is '), write(MaxE), write('.'), nl.
visit_dealer :-
        energy(E),
        E >= 50,
        max_energy(MaxE),
        NewMaxE is MaxE - 30,
        retract(max_energy(MaxE)),
        assert(max_energy(NewMaxE)),
        retract(energy(E)),
        NewE is E - 50,
        assert(energy(NewE)),
        write('You have bought a magic flute. You have '), write(NewE), write(' energy left.'), nl,
        assert(holding(magic_flute)).
ignore_dealer:-
        write('You hear a voice : "You will regret it!"'), nl.
fight_aligator:-
        energy(E),
        retract(energy(E)),
        NewE is E - 50,
        assert(energy(NewE)),
        write('You have fought with an aligator. You have '), write(NewE), write(' energy left.'), nl,
        (NewE =< 0 ->write('You are out of energy') ,die ; true),
        (NewE >= 0 -> improve_resting ; true).
use_flute:-
        holding(magic_flute),
        i_am_at(aligator_room),
        write(' You have used a magic flute. Aligator is obey. You can use him as a form of transport.'), nl,
        drive_aligator.