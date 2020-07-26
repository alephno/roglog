on_screen(X, Y, N, M) :-
    X >= 0,
    Y >= 0,
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

tl_inside(X1, Y1, X2, Y2, _, _, N2, M2) :-
    X1 >= X2,
    X1 =< X2 + M2,
    Y1 >= Y2,
    Y1 =< Y2 + N2. 
tr_inside(X1, Y1, X2, Y2, _, M1, N2, M2) :-
    X1 + M1 >= X2,
    X1 + M1 =< X2 + M2,
    Y1 >= Y2,
    Y1 =< Y2 + N2.
bl_inside(X1, Y1, X2, Y2, N1, _, N2, M2) :-
    X1 >= X2,
    X1 =< X2 + M2,
    Y1 + N1 >= Y2,
    Y1 + N1 =< Y2 + N2.
br_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) :-
    X1 + M1 >= X2,
    X1 + M1 =< X2 + M2,
    Y1 + N1 >= Y2,
    Y1 + N1 =< Y2 + N2.
intersects_any((_, _), []) :- false.
intersects_any((Coord1, Room1), [(Coord2, Room2)|Rest]) :-
    (X1, Y1) = Coord1,
    (N1, M1) = Room1,
    (X2, Y2) = Coord2,
    (N2, M2) = Room2,
    (tl_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) ;
     bl_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) ;
     tr_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) ;
     br_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) ;
     intersects_any((Coord1, Room1), Rest)).
    

valid_rooms([]) :- true.
valid_rooms([(Coord, Room)|Rest]) :-
    (X, Y) = Coord,
    (N, M) = Room,
    on_screen(X, Y, N, M),
    \+ intersects_any((Coord, Room), Rest),
    valid_rooms(Rest).

random_rooms(0, _, _, []).
random_rooms(Count, MinSize, MaxSize, [(Coord, Room)|Rest]) :-
    Count > 0,
    random(MinSize, MaxSize, N),
    random(MinSize, MaxSize, M),
    Room = (N, M),
    MaxX is 80 - MaxSize - 1,
    MaxY is 24 - MaxSize - 1,
    random(0, MaxX, X),
    random(0, MaxY, Y),
    Coord = (X, Y),
    NewCount is Count - 1,
    random_rooms(NewCount, MinSize, MaxSize, Rest).

level(MinRooms, MaxRooms, MinRoomSize, MaxRoomSize, Rooms) :-
    random(MinRooms, MaxRooms, N),
    random_rooms(N, MinRoomSize, MaxRoomSize, R),
    ((valid_rooms(R), Rooms = R) ;
     (\+valid_rooms(R),
      level(MinRooms, MaxRooms, MinRoomSize, MaxRoomSize, Redo),
      Rooms = Redo)).

draw_room_at(Coord, Room, X, Y) :-
    (Xoffset, Yoffset) = Coord,
    (N, M) = Room,
    tile(X, Y, N, M, C),
    TermX is Xoffset + X - 1,
    TermY is Yoffset + Y - 1,
    tty_goto(TermX, TermY),
    tty_put(C, 1),
    ((X < M, X1 is X +1, draw_room_at(Coord, Room, X1, Y)) ;
     (X is M, Y < N, Y1 is Y + 1, draw_room_at(Coord, Room, 1, Y1)) ;
     (X is M, Y is N)).
    
draw_room_at(Coord, Room) :-
    draw_room_at(Coord, Room, 1, 1).

draw_rooms([]).
draw_rooms([(Coord, Room)|Rest]) :-
    draw_room_at(Coord, Room),
    draw_rooms(Rest).

draw_level() :-
    tty_clear,
    level(5, 10, 5, 10, Rooms),
    draw_rooms(Rooms),
    tty_goto(0, 24).
    
