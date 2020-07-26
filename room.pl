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

row(_, M, X, _, []) :- X > M.
row(N, M, X, R, [H|T]) :-
  tile(N, M, X, R, H),
  X1 is X + 1,
  row(N, M, X1, R, T).

row(N, M, R, T) :-
  row(N, M, 1, R, T).

room(N, _, Y, []) :- Y > N.
room(N, M, Y, [H|T]) :-
  row(N, M, Y, H),
  Y1 is Y + 1,
  room(N, M, Y1, T).

room(N, M, T) :-
  room(N, M, 1, T).

write_at(C, X, Y) :-
  tty_goto(X, Y),
  tty_put(C, 1).

draw_row(M, X, _, []) :- X > M.
draw_row(M, X, Y, [H|T]) :-
  write_at(H, X, Y),
  X1 is X + 1,
  draw_row(M, X1, Y, T).

draw_room(N, _, _, Y, []) :- Y > N.
draw_room(N, M, _, Y, [H|T]) :-
  draw_row(M, 1, Y, H),
  Y1 is Y + 1,
  draw_room(N, M, 1, Y1, T).

draw_room(Tiles) :-
  length(Tiles, N),
  [Row|_] = Tiles,
  length(Row, M),
  draw_room(N, M, 1, 1, Tiles).

draw_random_room() :-
  random(5, 12, N),
  random(5, 12, M),
  room(N, M, Tiles),
  tty_clear,
  draw_room(Tiles).
