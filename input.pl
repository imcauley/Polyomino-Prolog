main(Filename) :-
    open(Filename, read, Str),
    read(Str,W),
    read(Str,H),
    read(Str,S),
    alphabet(A),
    getPieces(Str,S,A,P),
    close(Str), !.



getPieces(_,0,_,[]).
getPieces(Str,N,[A|As],[H|T]) :- read(Str, Size), getPiece(Str,Size,A,H),
                          M is N - 1, getPieces(Str,M,As,T).

getPiece(_,0,_,[]).
getPiece(Str,N,C,[(C,X,Y)|T]) :- read(Str,X), read(Str,Y), M is N - 1, getPiece(Str,M,C,T).

alphabet(['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O',
          'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).
