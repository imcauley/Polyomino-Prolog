boardSize(3,4).
board([]).
piece1([('A',0,0), ('A',1,0)]).
piece2([('B',0,0), ('B',1,0)]).
pieces([P1, P2]) :- piece1(P1), piece2(P2).

% solve will take in a list of pieces and return a board where
%  all pieces cannot be placed on the board

solve([],B,B,_).
solve([P|T],B,B3,(W,H)) :- member(X,[0,1,2,3,4]),
                           member(Y,[0,1,2,3,4]),
                           translate(X,Y,P,PieceT),
                           place(B,PieceT,B2),
                           solve(T,B2,B3,(W,H)).

translate(_,_,[],[]).
translate(X,Y,[(A,PX,PY)|T], [(A,PX2,PY2)|T2]) :- PX2 is PX + X,
                                                  PY2 is PY + Y,
                                                  translate(X,Y,T,T2).

inBounds(_,_,[]).
inBounds(X,Y,[(_,PX,PY)|T]) :- PX >= 0, PY >= 0,
                               PX < X, PY < Y,
                               inBounds(X,Y,T).

place(B,P,B2) :- fits(B,P) -> append(B,P,B2) ; B2 = B.

fits(_,[]).
fits(B,[H|T]) :- notOnBoard(B,H), fits(B,T).

notOnBoard([],_).
notOnBoard([(_,BX,BY)|T], (P,PX,PY)) :- (BX \= PX ; BY \= PY), notOnBoard(T,(P,PX,PY)).

makeList(0,[0]).
makeList(M,[Q|L]) :- Q is M - 1, N is M - 2, makeList(N, L).
