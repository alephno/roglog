:- module(path, [path/3]).
:- use_module(library(clpfd)).
:- use_module(utils).

connected(A, B) :-
    in_bounds(A),
    in_bounds(B),
    (X1, Y1) = A,
    (X2, Y2) = B,
    Y1 #= Y2,
    (X1 - X2 #= 1 ; X2 - X1 #= 1).
connected(A, B) :-
    in_bounds(A),
    in_bounds(B),
    (X1, Y1) = A,
    (X2, Y2) = B,
    X1 #= X2,
    (Y1 - Y2 #= 1 ; Y2 - Y1 #= 1).


unvisited_neighbors([], _, []).
unvisited_neighbors([N|Ns], Visited, Cs) :-
    member(N, Visited),
    unvisited_neighbors(Ns, Visited, Cs).
unvisited_neighbors([N|Ns], Visited, [C|Cs]) :-
    \+member(N, Visited),
    C = N,
    unvisited_neighbors(Ns, [N|Visited], Cs).

unvisited_connections(A, Visited, C) :-
    findall(B, connected(A, B), Neighbors),
    unvisited_neighbors(Neighbors, Visited, C).

connection_map([], _, [], []).
connection_map([Q|Qs], Visited, Queue, [M|Ms]) :-
    unvisited_connections(Q, Visited, Cons),
    M = (Q, Cons),
    append(Cons, NQs, Queue),
    append(Cons, Visited, NewVisited),
    connection_map(Qs, NewVisited, NQs, Ms).

connections(Queue, End, _, []) :-
    member(End, Queue).
connections(Queue, End, Visited, Map) :-
    \+member(End, Queue),
    connection_map(Queue, Visited, NewQueue, M),
    append(Ms, M, Map),
    append(Queue, Visited, NewVisited),
    connections(NewQueue, End, NewVisited, Ms).

path_from_map(_, [], []).
path_from_map(Coord, [(_, Neighbors)|Rest], P) :-
    \+member(Coord, Neighbors),
    path_from_map(Coord, Rest, P).
path_from_map(Coord, [(Origin, Neighbors)|Rest], [P|Ps]) :-
    member(Coord, Neighbors),
    P = (Origin, (0,0)),
    path_from_map(Origin, Rest, Ps).

path(Start, End, Path) :-
    once(connections([Start], End, [], Map)),
    path_from_map(End, Map, P),
    reverse([End|P], Path).
    
