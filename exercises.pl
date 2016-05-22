long([], 0).
long([_|T], X+1) :- long(T, X).

removeConsecutiveRepeated([X],[X]).
removeConsecutiveRepeated([X,X|XS],[X|RS]) :- removeConsecutiveRepeated([X|XS],[X|RS]).
removeConsecutiveRepeated([X,Y|XS],[X,Y|RS]) :- removeConsecutiveRepeated([Y|XS],[Y|RS]), X \= Y.

%prefix(+List,?Prefix)
prefix(L1, L2) :- append(L1,_, L2).

%sufix(+Lista,?Sufix)
sufix(L1, L2) :- append(_, L1, L2).

%sublist(+Lista,?Subl)
sublist(L1, L2) :- sufix(LprefAndL1, L1), prefix(Lpref, LprefAndL1)

%$Lpref | L1 = LprefAndL1
%LprefAndL1 | Lsuf = L2