corner(_, _, X, Y) :- X = 1, Y = 1.
corner(N, M, X, Y) :- X = M, Y = N.
corner(N, _, X, Y) :- X = 1, Y = N.
corner(_, M, X, Y) :- X = M, Y = 1.

v_wall(N, _, X, Y) :- X = 1, Y > 1, Y < N.
v_wall(N, M, X, Y) :- X = M, Y > 1, Y < N.

h_wall(_, M, X, Y) :- Y = 1, X > 1, X < M.
h_wall(N, M, X, Y) :- Y = N, X > 1, X < M.

floor(N, M, X, Y) :- X > 1, X < M, Y > 1, Y < N.

tile(N, M, X, Y, C) :- 
  (floor(N, M, X, Y), C = '.') ;
  (v_wall(N, M, X, Y), C = '|') ;
  (h_wall(N, M, X, Y), C = '-') ;
  (corner(N, M, X, Y), C = ' ').

row_rec(_, M, X, _, []) :- X > M.
row_rec(N, M, X, R, [H|T]) :-
  tile(N, M, X, R, H),
  X1 is X + 1,
  row_rec(N, M, X1, R, T).

row(N, M, R, T) :-
  row_rec(N, M, 1, R, T).

room_rec(N, _, Y, []) :- Y > N.
room_rec(N, M, Y, [H|T]) :-
  row(N, M, Y, H),
  Y1 is Y + 1,
  room_rec(N, M, Y1, T).

room(N, M, L) :-
  room_rec(N, M, 1, L).

writeAt(C, X, Y) :-
  tty_goto(X, Y),
  tty_put(C, 1).

drawRow(M, X, _, []) :- X > M.
drawRow(M, X, Y, [H|T]) :-
  writeAt(H, X, Y),
  X1 is X + 1,
  drawRow(M, X1, Y, T).

draw_room_rec(N, _, _, Y, []) :- Y > N.
draw_room_rec(N, M, _, Y, [H|T]) :-
  drawRow(M, 1, Y, H),
  Y1 is Y + 1,
  draw_room_rec(N, M, 1, Y1, T).

draw_room(Tiles) :-
  length(Tiles, N),
  [Row|_] = Tiles,
  length(Row, M),
  draw_room_rec(N, M, 1, 1, Tiles).

draw_random_room() :-
  random(3, 8, N),
  random(3, 8, M),
  room(N, M, Tiles),
  tty_clear,
  draw_room(Tiles).
  