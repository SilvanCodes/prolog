:- dynamic here/1.
:- dynamic location/2.
:- dynamic have/1.
:- dynamic turned_off/1.
:- dynamic turned_on/1.

:- op(100, fx, goto).
:- op(100, fx, take).
:- op(100, fx, put).
:- op(100, fx, turn_on).
:- op(100, fx, turn_off).

room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).

here(kitchen).

door(office, hall).
door(kitchen, office).
door(hall, 'dining room').
door(kitchen, cellar).
door('dining room', kitchen).

location(object(candle, color(red), size(small), weight(1)), kitchen).
location(object(apple, color(red), size(small), weight(1)), kitchen).
location(object(apple, color(green), size(small), weight(1)), kitchen).
location(object(table, color(blue), size(big), weight(50)), kitchen).
location(object(desk, color(brown), size(big), weight(90)), office).
location(object(flashlight, color(black), size(small), weight(2)), desk).
location(object('washing machine', color(white), size(big), weight(70)), cellar).
location(object(nani, color(yellow), size(small), weight(1)), 'washing machine').
location(object(broccoli, color(green), size(small), weight(1)), kitchen).
location(object(crackers, color(brown), size(small), weight(1)), kitchen).
location(object(computer, color(grey), size(big), weight(10)), office).
location(object(envelope, color(white), size(small), weight(1)), desk).
location(object(stamp, color(blue), size(small), weight(1)), envelope).
location(object(key, color(golden), size(small), weight(1)), envelope).

list_things(Place) :-  
    location(Object, Place),
    tab(2),
    write_object(Object),
    fail.
list_things(_).

write_weight(1) :-
    write('1 pound').
write_weight(W) :-
    W > 1,
    write(W), write(' pounds').

open(door(office, hall)).
open(door(kitchen, office)).
open(door(hall, 'dining room')).
open(door('dining room', kitchen)).

closed(door(kitchen, cellar)).

edible(apple).
edible(crackers).

tastes_yucky(broccoli).

turned_off(flashlight).

turned_off(Thing) :-
    write(Thing),
    write(' is turned on.'),
    nl,
    fail.

turned_on(Thing) :-
    write(Thing),
    write(' is turned off.'),
    nl,
    fail.

where_food(X, Y) :-
    location(object(X, _, _, _), Y),
    edible(X).
where_food(X, Y) :-
    location(object(X, _, _, _), Y),
    tastes_yucky(X).

connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).

list_connections(Place) :-
    connect(Place, X),
    tab(2),
    write(X),
    nl,
    fail.
list_connections(_).

look :-
    here(Place),
    write('You are in the '),
    write(Place),
    nl,
    write('You can see:'),
    nl,
    list_things(Place),
    write('You can go to:'),
    nl,
    list_connections(Place).

goto(Place) :-
    can_go(Place),
    move(Place),
    look,
    !.

can_go(Place) :-
    here(X),
    connect(X, Place),
    is_open(X, Place).
can_go(Place) :-
    here(Place),
    write("This is were you are."),
    nl,
    fail.
can_go(Place) :-
    not(here(Place)),
    write("You can't get there from here."),
    nl,
    fail.

is_open(Place1, Place2) :-
    open(door(Place1, Place2));
    open(door(Place1, Place2)).
is_open(_, _) :-
    write('The door is closed.'),
    nl,
    fail.

move(Place) :-
    retract(here(_)),
    asserta(here(Place)).
    
take(Thing) :-
    can_take(Thing),
    take_object(Thing),
    !.

can_take(Thing) :-
    here(Place),
    location(object(Thing, _, size(small), _), Place).
can_take(Thing) :-
    here(Place),
    location(object(Thing, _, size(big), _), Place),
    write('The '),
    write(Thing), 
    write(' is too big to carry.'),
    nl,
    fail.
can_take(Thing) :-
    here(Place),
    not(location(object(Thing, _, _, _), Place)),
    write('There is no '),
    write(Thing),
    write(' here.'),
    nl,
    fail.

take_object(Thing) :-
    retract(location(object(Thing, C, S, W), _)),
    asserta(have(object(Thing, C, S, W))),
    write('taken'),
    nl.

put(Thing) :-
    can_put(Thing),
    put_object(Thing).

can_put(Thing) :-
    have(object(Thing, _, _, _)).
can_put(Thing) :-
    write("You don't have "),
    write(Thing),
    nl,
    fail.

put_object(Thing) :-
    here(Place),
    retract(have(object(Thing, C, S, W))),
    asserta(location(object(Thing, C, S, W), Place)),
    write('put'),
    nl.

inventory :-
    not(have(_)),
    write("You dont have a thing."),
    nl.
inventory :-
    have(Object),
    tab(2),
    write_object(Object),
    fail.
inventory.

write_object(object(Thing, color(C), size(S), weight(W))) :-
    write('A '),
    write(S),
    tab(1),
    write(C),
    tab(1),
    write(Thing),
    write(', weighing '),
    write_weight(W),
    nl.

turn_on(Thing) :-
    can_turn_on(Thing),
    do_turn_on(Thing).

can_turn_on(Thing) :-
    have(Thing),
    turned_off(Thing).

do_turn_on(Thing) :-
    retract(turned_off(Thing)),
    asserta(turned_on(Thing)),
    write(Thing),
    write(' turned on.'),
    nl.

turn_off(Thing) :-
    can_turn_on(Thing),
    do_turn_on(Thing).

can_turn_off(Thing) :-
    have(Thing),
    turned_on(Thing).

do_turn_off(Thing) :-
    retract(turned_on(Thing)),
    asserta(turned_off(Thing)),
    write(Thing),
    write(' turned off.'),
    nl.

is_contained_in(T1,T2) :-
    location(object(T1, _, _, _), T2).
is_contained_in(T1, T2) :-
    location(object(X, _, _, _), T2),
    is_contained_in(T1, X).

respond([]).
respond([H|T]) :-
    write(H),
    respond(T).

puzzle(goto(cellar)):-
    have(flashlight),
    turned_on(flashlight),
    !.
puzzle(goto(cellar)):-
    write("It's dark and you are afraid of the dark."),
    !,
    fail.
puzzle(_).

command_loop:- 
    write('Welcome to Nani Search'),
    nl,
    repeat,
    write('>nani> '),
    read(X),
    puzzle(X),
    do(X), nl,
    end_condition(X).

end_condition(end).
end_condition(_) :-
  have(nani),
  write('Congratulations').

do(goto(X)) :-
    goto(X),
    !.
do(go(X)) :-
    goto(X),
    !.
do(inventory) :-
    inventory,
    !.
do(look) :-
    look,
    !.
do(take(X)) :-
    take(X),
    !.
do(end).
do(_) :-
  write('Invalid command').