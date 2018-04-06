%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog Polyomino Solver
% Isaac McAuley
% For Charlie Hepler
% COMP 3649
% Assignment 3
%
% Last Edited: April 6, 2018
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Filename) :-
    open(Filename, read, Str),
    getInt(Str,W),
    getInt(Str,H),
    getInt(Str,S),
    alphabet(A),
    getPieces(Str,S,A,P),
    close(Str),
    (solve(P,[],B,(W,H)) -> makeBoard(H,W,Empty),
                            createBoard(Empty,Full,B),
                            printBoard(Full)

                            ; write('No solution')),
    !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLUTON FINDER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve([],B,B,_).
solve([P|T],B,B3,(W,H)) :- numlist(0,W,Xlist),
                           numlist(0,H,Ylist),
                           member(X,Xlist),
                           member(Y,Ylist),
                           pieceCollection(P,PS),
                           member(P1,PS),
                           translate(X,Y,P1,PieceT),
                           inBounds(W,H,PieceT),
                           place(B,PieceT,B2),
                           solve(T, B2, B3, (W,H)), !.


translate(_,_,[],[]).
translate(X,Y,[(A,PX,PY)|T], [(A,PX2,PY2)|T2]) :- PX2 is PX + X,
                                                  PY2 is PY + Y,
                                                  translate(X,Y,T,T2).

inBounds(_,_,[]).
inBounds(X,Y,[(_,PX,PY)|T]) :- PX >= 0, PY >= 0,
                               PX < X, PY < Y,
                               inBounds(X,Y,T).

place(B,P,B2) :- fits(B,P) -> append(B,P,B2) ; false.

fits(_,[]).
fits(B,[H|T]) :- notOnBoard(B,H), fits(B,T).

notOnBoard([],_).
notOnBoard([(_,BX,BY)|T], (P,PX,PY)) :- (BX \= PX ; BY \= PY),
                                         notOnBoard(T,(P,PX,PY)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PIECE MANIPULATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rotatePiece(P, P2) :- maplist(rotate, P, P2).
rotate((A, X1, Y1), (A, X2, Y2)) :- X2 is Y1, Y2 is (-X1).

flipPiece(P, P2) :- maplist(flip, P, P2).
flip((A, X1, Y1), (A, X1, Y2)) :- Y2 is ((-Y1) + 1).

pieceCollection(P, [P,PR,PRR,PRRR,
                    PF,PFR,PFRR,PFRRR]) :- rotatePiece(P,PR),
                                            rotatePiece(PR,PRR),
                                            rotatePiece(PRR,PRRR),
                                            flipPiece(P,PF),
                                            flipPiece(PR,PFR),
                                            flipPiece(PRR,PFRR),
                                            flipPiece(PRRR,PFRRR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INPUT / OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeBoard(_,0,[]) :- !.
makeBoard(N,M,[B|T]) :- makeString('#', N, B), M1 is M - 1, makeBoard(N, M1, T).

makeString(_,0,[]).
makeString(C,N,[C|T]) :- N1 is N - 1, makeString(C,N1,T).



printBoard([]).
printBoard([R|C]) :- printRow(R), printBoard(C).

printRow([]) :- write('\n').
printRow([C|R]) :- write(C), printRow(R).



replaceInBoard(A,0,R,[H|T],[N|T]) :- replaceInRow(A,R,H,N).
replaceInBoard(A,C,R,[H|T],[H|N]) :- C1 is C - 1, replaceInBoard(A,C1,R,T,N).

replaceInRow(C,0,[_|T], [C|T]).
replaceInRow(C,N,[H|T], [H|R]) :- M is N - 1, replaceInRow(C, M, T, R).


createBoard(B1,B1,[]) :- !.
createBoard(B1,B3,[(A,X,Y)|T]) :- replaceInBoard(A,X,Y,B1,B2),
                                  createBoard(B2,B3,T).

getPieces(_,0,_,[]).
getPieces(Str,N,[A|As],[H|T]) :- getInt(Str, Size), getPiece(Str,Size,A,H),
                          M is N - 1, getPieces(Str,M,As,T).

getPiece(_,0,_,[]).
getPiece(Str,N,C,[(C,X,Y)|T]) :- getInt(Str,X), getInt(Str,Y),
                                 M is N - 1, getPiece(Str,M,C,T).

getInt(Str, N) :- getDigits(Str,D, true), atom_string(D,S), atom_number(S,N).

is_int(C) :- member(C, ['0','1','2','3','4','5','6','7','8','9']).

getDigits(Str,[C|T], _) :- get_char(Str, C), is_int(C), getDigits(Str,T, false).
getDigits(Str, T, true) :- getDigits(Str,T, true).
getDigits(_, [], false) :- !.

alphabet(['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O',
          'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).
