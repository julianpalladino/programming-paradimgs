%long(+L,-Res)
%long([],cero).
%long(.(_,XS), suc(Rec)) :- long(XS,Rec).

long([], 0).
long([_|T], X+1) :- long(T, X).

src([X],[X]).
src([X,X|XS],[X|RS]) :- src([X|XS],[X|RS]).
src([X,Y|XS],[X,Y|RS]) :- src([Y|XS],[Y|RS]), X \= Y.