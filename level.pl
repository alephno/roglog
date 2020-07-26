
on_screen(X, Y, N, M) :-
    X > 0,
    Y > 0,
    X + M < 80,
    Y + N < 24.

corner(_, _, X, Y) :- X = 1, Y = 1.
corner(N, M, X, Y) :- X = M, Y = N.
corner(N, _, X, Y) :- X = 1, Y = N.
corner(_, M, X, Y) :- X = M, Y = 1.

v_wall(N, _, X, Y) :- X = 1, Y > 1, Y < N.
v_wall(N, M, X, Y) :- X = M, Y > 1, Y < N.

h_wall(_, M, X, Y) :- Y = 1, X > 1, X < M.
h_wall(N, M, X, Y) :- Y = N, X > 1, X < M.

floor(N, M, X, Y) :- X > 1, X < M, Y > 1, Y < N.

tile(X, Y, N, M, C) :- 
  (floor(N, M, X, Y), C = '.') ;
  (v_wall(N, M, X, Y), C = '|') ;
  (h_wall(N, M, X, Y), C = '-') ;
  (corner(N, M, X, Y), C = ' ').

tiles(X, Y, N, M, [H|[]]) :- X is M, Y is N, tile(X, Y, N, M, H).
tiles(X, Y, N, M, [H|T]) :-
    X =< M,
    Y =< N,
    tile(X, Y, N, M, H),
    ((X < M, X1 is X + 1, tiles(X1, Y, N, M, T)) ;
     (X is M, Y < N, Y1 is Y + 1, tiles(1, Y1, N, M, T))).
    

room(N, M, Tiles) :-
    N >= 3,
    M >= 3,
    tiles(1, 1, N, M, Tiles).

intersects_any((_, _), []) :- false.
intersects_any((Coord1, Room1), [(Coord2, Room2)|Rest]) :-
    (X1, Y1) = Coord1,
    room(N1, M1, _) = Room1,
    (X2, Y2) = Coord2,
    room(N2, M2, _) = Room2,
    X1 < X2 + M2,
    X1 + M1 > X2,
    Y1 < Y2 + N2,
    Y1 + N1 < Y2,
    intersects_any((Coord1, Room1), Rest).

valid_rooms([]) :- true.
valid_rooms([(Coord, Room)|Rest]) :-
    (X, Y) = Coord,
    (N, M) = Room,
    on_screen(X, Y, N, M),
    \+ intersects_any((Coord, Room), Rest),
    valid_rooms(Rest).

random_rooms(0, []).
random_rooms(Count, [(Coord, Room)|Rest]) :-
    Count > 0,
    random(3, 5, N),
    random(3, 5, M),
    Room = (N, M),
    random(0, 74, X),
    random(0, 18, Y),
    Coord = (X, Y),
    NewCount is Count - 1,
    random_rooms(NewCount, Rest).

level(Rooms) :-
    random(3, 5, N),
    random_rooms(N, Rooms),
    valid_rooms(Rooms).
