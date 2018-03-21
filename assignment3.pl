board([(0,0), (2,4)]).
piece1([(1,1)]).

fits(_,[]).
fits(B, [H|T]) :- not(member(H,B)), fits(B,T).

place(B,P,B2) :- append(B,P,B2). 
