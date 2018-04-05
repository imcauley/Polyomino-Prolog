main :-
    open('6.txt', read, Str),
    getInt(Str,W),
    getInt(Str,H),
    getInt(Str,S),
    alphabet(A),
    getPieces(Str,S,A,P),
    write(W), nl,
    write(H), nl,
    write(P), nl, !.




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
