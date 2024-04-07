/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1, energy/1, max_energy/1, resting_pace/1, alive/1, travelling_cost/1, rats_defeated/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(entrance).

path(entrance, forward, hall).

path(hall, right, in_front_of_first_tunnel).
path(hall, forward, in_front_of_second_tunnel).
path(hall, left, in_front_of_third_tunnel).

path(in_front_of_first_tunnel, forward, first_tunnel).
path(in_front_of_second_tunnel, forward, second_tunnel).
path(in_front_of_third_tunnel, forward, third_tunnel).

path(in_front_of_first_tunnel, back, hall).
path(in_front_of_second_tunnel, back, hall).
path(in_front_of_third_tunnel, back, hall).

path(in_front_of_first_tunnel, left, in_front_of_second_tunnel).
path(in_front_of_second_tunnel, right, in_front_of_first_tunnel).
path(in_front_of_second_tunnel, left, in_front_of_third_tunnel).
path(in_front_of_third_tunnel, right, in_front_of_second_tunnel).

path(first_tunnel, back, in_front_of_first_tunnel).
path(first_tunnel, forward, dealer_room).
/*TODO: Finish navigation for first tunnel */

path(second_tunnel, back, in_front_of_second_tunnel).
path(second_tunnel, forward, tunnel_diggers).
path(tunnel_diggers, back, second_tunnel).
path(tunnel_diggers, forward, second_tunnel_1).
path(tunnel_diggers, left, side_tunnnel).
path(side_tunnnel, back, tunnel_diggers).
path(second_tunnel_1, back, tunnel_diggers).

path(third_tunnel, back, in_front_of_third_tunnel).

path(dealer_room, forward, aligator_room).
path(aligator_room, forward, waterfall).

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

take(flute) :- 
        energy(E),
        E >= 50,
        max_energy(MaxE),
        NewMaxE is MaxE - 30,
        retract(max_energy(MaxE)),
        assert(max_energy(NewMaxE)),
        retract(energy(E)),
        NewE is E - 50,
        assert(energy(NewE)),
        write('You have bought a magic flute. You have '), write(NewE), write(' energy left.'),
        assert(holding(magic_flute)),
        nl.

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

/* These rules describe inspection of an object */
inspect(X) :-
        holding(X),
        describe(X).

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
        write('go(Direction)      -- to go in that direction.'), nl,
        write('Available directions: forward, back, left, right'), nl,
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
describe(in_front_of_first_tunnel) :- write('You are standing in front of the rightmost tunnel.'), nl.
describe(in_front_of_second_tunnel) :- write('You are standing in front of the middle tunnel.'), nl.
describe(in_front_of_third_tunnel) :- write('You are standing in front of the leftmost tunnel'), nl.

describe(first_tunnel) :- write('You are in the first tunnel. You can smell blood inside. It seems to be a bad way to escape.'), nl.

describe(second_tunnel) :- write('You are in the second tunnel. You can see a light at the end of a tunnel. Maybe this is a way to escape?'), nl.

describe(second_tunnel_1) :- holding(aligator), write('You reach the end of the tunnel. The light you saw comes from a tiny hole in the left wall of the tunnel.'), nl.
describe(second_tunnel_1) :-
        retractall(energy(_)),
        assert(energy(0)), 
        write('You reach the end of the tunnel. The light you saw comes from a tiny hole in the left wall of the tunnel. You have walked for so long that you ran out of energy.'),
        die,
        nl.

describe(third_tunnel) :- write('You cannot open the door. There is no place to insert a key. Maybe a magic item can open them?'), nl.
describe(dealer_room) :- write('You have entered a Jewish dealer''s space. He wants to sell you a magic flute, but he do not specified its aim. Maybe it can be useful? He wants to help him, it will cost you 60 energy. (use take(flute) to buy the flute)'), nl.
describe(aligator_room) :- write('You have entered an Aligator space. There is a huge reptile at the back. Fight could be difficult and demanding. Would you try?'), nl.
describe(waterfall) :- write('You have entered a waterfall. You can see a light at the end of the tunnel. You are carried away by the current of water.'), die, nl.
describe(note) :- write('You read the note from a lost wanderer. It says: You are in a maze. You need to find a way out. There are 3 tunnels. The first one is very dangerous. The second one has a light at the end - thats the path you should take. The third one may hold a secret. Choose wisely. (Press ENTER to continue)'), nl.
describe(aligator) :- write('The aligator is huge, and its scales glisten in the dimmly lit tunnel.'), nl.

describe(tunnel_diggers) :-
        holding(stone_tablet_1),
        rats_defeated(true),
        write('The miners happily greet you. They offer you a broken half of a stone tablet as thanks. You can feel magical energy from it.'), nl,
        write('Would you like to take the tablet? (Use take(tablet_2) to take it)'),
        nl.


describe(tunnel_diggers) :-
        rats_defeated(true),
        write('You stand at the entrance to the side tunnel. The miners have moved back into the side tunnel to keep digging.'),
        nl.

describe(tunnel_diggers) :- write('You see a few people with mining tools. They tell you that they''re trying to dig a tunnel to the surface but a herd of rats stands in their way. Prehaps they will help you if you slay the rats?'), nl.

rest :-
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

ignore_dealer:-
        write('You hear a voice : "You will regret it!"'), nl.

fight:-
        energy(E),
        retract(energy(E)),
        NewE is E - 50,
        assert(energy(NewE)),
        write('You have fought with an aligator. You have '), write(NewE), write(' energy left.'), nl,
        (NewE =< 0 ->write('You are out of energy') ,die ; true),
        (NewE >= 0 -> improve_resting ; true).
