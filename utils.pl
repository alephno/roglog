:- module(utils, [term_width/1, term_height/1, in_bounds/1, in_bounds/2, intersects_any/2]).
:- use_module(library(clpfd)).

term_width(X) :- X #= 80.
term_height(Y) :- Y #= 24.

in_bounds((X, Y), (N, M)) :-
    term_width(W),
    term_height(H),
    X #>= 1,
    X + M #=< W,
    Y #>= 1,
    Y + N #=< H.
in_bounds(P) :-
    in_bounds(P, (0,0)).


tl_inside(X1, Y1, X2, Y2, _, _, N2, M2) :-
    X1 #>= X2,
    X1 #=< X2 + M2,
    Y1 #>= Y2,
    Y1 #=< Y2 + N2. 
tr_inside(X1, Y1, X2, Y2, _, M1, N2, M2) :-
    X1 + M1 #>= X2,
    X1 + M1 #=< X2 + M2,
    Y1 #>= Y2,
    Y1 #=< Y2 + N2.
bl_inside(X1, Y1, X2, Y2, N1, _, N2, M2) :-
    X1 #>= X2,
    X1 #=< X2 + M2,
    Y1 + N1 #>= Y2,
    Y1 + N1 #=< Y2 + N2.
br_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) :-
    X1 + M1 #>= X2,
    X1 + M1 #=< X2 + M2,
    Y1 + N1 #>= Y2,
    Y1 + N1 #=< Y2 + N2.
intersects(X1, Y1, X2, Y2, N1, M1, N2, M2) :-
    tl_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) ;
     bl_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) ;
     tr_inside(X1, Y1, X2, Y2, N1, M1, N2, M2) ;
     br_inside(X1, Y1, X2, Y2, N1, M1, N2, M2).
intersects_any((_, _), []) :- false.
intersects_any((Coord1, Room1), [(Coord2, Room2)|Rest]) :-
    (X1, Y1) = Coord1,
    (N1, M1) = Room1,
    (X2, Y2) = Coord2,
    (N2, M2) = Room2,
    ((intersects(X1, Y1, X2, Y2, N1, M1, N2, M2)) ;
     (intersects(X2, Y2, X1, Y1, N2, M2, N1, M1)) ;
     intersects_any((Coord1, Room1), Rest)).
